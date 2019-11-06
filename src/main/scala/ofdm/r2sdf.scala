package ofdm
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.internal.requireIsChiselType
import chisel3.util._
import dsptools.numbers._
import dsptools._

import scala.math.{Pi, cos, sin}

case class FFTParams[T <: Data](
  proto: T, 
  N: Int,
  DIT: Boolean,
  inverse: Boolean = false,
  scale: Boolean = true,
  name: String = "FFT_R2SDF") {
    requireIsChiselType(proto,  s"genIn ($proto) must be chisel type")
}

class BflyR22IO[T <: Data : Ring : BinaryRepresentation : ConvertableTo](gen: T) extends Bundle { 
  val din   = Input(DspComplex(gen))
  val dout  = Output(DspComplex(gen))
  val sel   = Input(Bool())
  val stall = Input(Bool())
}

class BflyR22[T <: Data : Ring : BinaryRepresentation : ConvertableTo](gen: T, N: Int) extends Module {
  val io = IO(new BflyR22IO(gen))
  val q_dout = Wire(DspComplex(gen)) 
  val q_din  = Wire(DspComplex(gen)) 
  val sum  = (q_dout + io.din)
  val diff = (q_dout - io.din)
  when (io.sel) {
    q_din := diff
  } .otherwise {
    q_din := io.din
  }

//  val q_dout  = ShiftRegister(Mux(!io.sel, io.din,   diff), N, !io.stall)
  q_dout := ShiftRegister(q_din, N, !io.stall)

//  io.dout   := Mux(!io.sel, q_dout, sum)
  when (io.sel) {
      io.dout := sum
  } .otherwise {
    io.dout := q_dout
  }

}

class R2SDF_TFMulIO[T <: Data : Ring : BinaryRepresentation: ConvertableTo](gen: T, N: Int) extends Bundle {
  val din   = Input(DspComplex(gen))
  val dout  = Output(DspComplex(gen))
  val addr  = Input(UInt((log2Ceil(N)+1).W))
}

// combined multiplier + twiddle factor lookup table
class R2SDF_TFMul[T <: Data : Ring : BinaryRepresentation : ConvertableTo](gen: T, N: Int, inverse: Boolean = false) extends Module {
  val io = IO(new R2SDF_TFMulIO(gen, N))
  val msb  = io.addr(log2Ceil(N))
  val addr = Mux(msb, 0.U, io.addr(log2Ceil(N)-1,0))
  val bpos = (gen match {
      case fp: FixedPoint => fp.binaryPoint.get 
      case _ => 0
  })

  // twiddle factor lookup table
  val tf = Wire(Vec(N, DspComplex(gen)))
  DspContext.withTrimType(RoundHalfUp) {
    for (k <- 0 until N) {
        tf(k).real := gen.fromDoubleWithFixedWidth(cos(2*Pi*(k.toDouble)/(2*N)))
      if (inverse) {
        tf(k).imag := gen.fromDoubleWithFixedWidth(sin(2*Pi*(k.toDouble)/(2*N)))
      } else {
        tf(k).imag := gen.fromDoubleWithFixedWidth(-sin(2*Pi*(k.toDouble)/(2*N)))
      }
    }
    val mulres = io.din * tf(addr)
//    when (addr === 0.U) {
//      io.dout := io.din
//    } .elsewhen (io.addr === (N/2).U) {
//      io.dout := (if (inverse) io.din.mulj() else io.din.divj())
//    } .otherwise {
//      io.dout := mulres.trimBinary(bpos)
//    }
    io.dout := mulres.trimBinary(bpos)
  }
}

class FFT_R2SDF_io[T <: Data : Ring : BinaryRepresentation : ConvertableTo](gen: T) extends Bundle {
  val din   = Flipped(Valid(DspComplex(gen)))
  val dout  = Valid(DspComplex(gen))
  val init  = Input(Bool())
  val stall = Input(Bool())
}

class FFT_R2SDF[T <: Data : Ring : BinaryRepresentation : ConvertableTo](params: FFTParams[T]) extends Module {
  val N = params.N
  val nstages = log2Ceil(N) 
  val gen = params.proto
  val width = gen.getWidth
  val bpos = (gen match {
      case fp: FixedPoint => fp.binaryPoint.get 
      case _ => 0
    })
  val wgen = FixedPoint((width + nstages).W, bpos.BP)

  val inverse = params.inverse
  val DIT = params.DIT
  assert(isPow2(N) & ((N%4)==0), "illegal parameter N = " + N + " passed to FFT_R2SDF.  N must be a power of 4")
  val io = IO(new FFT_R2SDF_io(gen))
  val s_dout_reg  = Seq.fill(nstages) { Reg(DspComplex(wgen)) }
  val s_dout  = Wire(Vec(nstages, DspComplex(wgen)))
  val en_regs = RegInit(VecInit(Seq.fill(nstages)(false.B)))
  val dcnt    = RegInit(VecInit(Seq.fill(nstages)(0.asUInt(nstages.W))))
  val cycles  = RegInit(0.asUInt((nstages+3).W))
  val din_reg = RegEnable(io.din.bits, !io.stall)

  when (io.init) {
    cycles := 0.U
  } .elsewhen (en_regs(nstages-1) & !io.stall) {
    cycles := Mux(cycles === N.U, cycles, cycles + 1.U)
  }

  val dout_good = (cycles === N.U)

  for (stage <- 0 to nstages-1) {
    when (io.init) {
      en_regs(stage) := false.B
      dcnt(stage) := (if (DIT) (N-2).U else 0.U)
    } .elsewhen (!io.stall) {
      s_dout_reg(stage) := s_dout(stage)
      en_regs(stage) := (if (stage == 0) io.din.valid else en_regs(stage-1))
      when (en_regs(stage)) {
        val dnext = (if (DIT) (dcnt(stage) - 1.U) else (dcnt(stage) + 1.U))
        dcnt(stage) := (if (stage == 0) dnext else dcnt(stage-1))
      }
    }
  }

  for (stage <- 0 to nstages-1) {
    val bf = Module(new BflyR22(wgen, (if (DIT) 1<<stage else (N/2)>>stage)))
    bf.io.sel   := dcnt(stage)((if (DIT) stage else nstages-1-stage))
    bf.io.stall := io.stall
    bf.io.din   := (if (stage == 0) din_reg else s_dout_reg(stage-1))
    if (stage == nstages-1) {
      s_dout(stage) :=  bf.io.dout
    } else { 
      val bfmul = Module(new R2SDF_TFMul(wgen, (if (DIT) 2<<stage else (N/2)>>stage), inverse))
      bfmul.io.din  := bf.io.dout
      bfmul.io.addr := (if (DIT) ~dcnt(stage)(stage+1,0) else dcnt(stage)(nstages-1-stage,0))
      s_dout(stage) := bfmul.io.dout
    } 
  }

  io.dout.valid := en_regs(nstages-1) & dout_good & !io.stall  
  if (params.scale) {
    io.dout.bits  := s_dout_reg(nstages-1).div2(nstages)
  } else {
    io.dout.bits  := s_dout_reg(nstages-1)
  }
}

object BuildSampleFFT {
  def main(args: Array[String]): Unit = {
    val params: FFTParams[FixedPoint] = FFTParams(
      FixedPoint(16.W, 14.BP),
      N = 64,
      DIT = true
    )
    chisel3.Driver.execute(Array("-X", "verilog"), () => new FFT_R2SDF(params))
  }
}
