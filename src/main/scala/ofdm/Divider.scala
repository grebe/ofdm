package ofdm

import chisel3._
import chisel3.util.{Cat, ShiftRegister, Valid}

class DivisionStageIO(val n: Int) extends Bundle {
  val pin = Input(UInt( (2*n).W ))
  val d = Input(UInt( (2*n).W ))

  val pout = Output(UInt( (2*n).W ))
  val qout = Output(UInt(1.W))
}

object SetBit {
  def apply(in: UInt, idx: Int, value: Bool): UInt = {
    val n = in.getWidth
    if (idx == 0) {
      Cat(in(n-1,1), value)
    } else if (idx == n - 1) {
      Cat(value, in(n-2, 0))
    } else {
      Cat(in(n - 1, idx + 1), value, in(idx - 1, 0))
    }
  }
  def apply(in: UInt, idx: UInt, value: Bool): UInt = {
    val mask: UInt = (1.U << idx).asTypeOf(in)
    val out = Wire(UInt())
    when (value) {
      out := in | mask
    }

    when (in(idx) && !value) {
      out := in ^ mask
    }

    out
  }
}

trait HasDivisionIO extends RawModule {
  def io: DivisionStageIO
}

class NonRestoringStage(n: Int) extends RawModule with HasDivisionIO {
  val io = IO(new DivisionStageIO(n))

  val pGtEqZero: Bool = io.pin(2*n-1) === 0.U
  val pShift: UInt = io.pin << 1

  io.qout := pGtEqZero
  io.pout := Mux(pGtEqZero, pShift -& io.d, pShift +& io.d)
}

class PipelinedDividerInputIO(val n: Int) extends Bundle {
  val num    = UInt(n.W)
  val denom  = UInt(n.W)
}

class PipelinedDividerIO(val n: Int) extends Bundle {
  val in = Input(Valid(new PipelinedDividerInputIO(n)))
  val out = Output(Valid(UInt(n.W)))
}

class RedundantToNonRedundant(n: Int) extends RawModule {
  val qin  = IO(Input(UInt(n.W)))
  val pin  = IO(Input(UInt((2*n).W)))
  val qout = IO(Output(UInt(n.W)))

  val correction = Mux(pin(2 * n - 1), 1.U, 0.U)

  qout := qin - (~qin).asUInt - correction
}

class PipelinedDivider(val n: Int, val conversionDelay: Int = 1) extends Module {
  require(n > 0)

  val io = IO(new PipelinedDividerIO(n))

  val stages = Seq.fill(n + 1) { Module(new NonRestoringStage(n + 1)) }
  val d: UInt = io.in.bits.denom << (n + 1)

  // we want qin to be zero-width so it doesn't add an extra MSB
  val qin0 = Wire(UInt(0.W))
  qin0 := 0.U

  val (p, q, _) = stages.foldLeft( (/*numAbs*/ io.in.bits.num, qin0, d) ) { case ( (pin, qin, din), stage) =>
    stage.io.d := din
    stage.io.pin := pin

    (RegNext(stage.io.pout), RegNext(Cat(qin, stage.io.qout)), RegNext(din))
  }

  val nonRedundantConverter = Module(new RedundantToNonRedundant(n + 1))
  nonRedundantConverter.qin := q
  nonRedundantConverter.pin := p

  val latency = stages.length + conversionDelay

  io.out.bits := ShiftRegister(/*signedQout*/ nonRedundantConverter.qout, conversionDelay)
  io.out.valid := ShiftRegister(io.in.valid, latency, resetData = false.B, en = true.B)
}
