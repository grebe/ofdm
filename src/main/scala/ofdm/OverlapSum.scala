package ofdm

import chisel3._
import chisel3.util._
import dsptools.numbers._

class OverlapSum[T <: Data : Ring](val gen: T, val maxDepth: Int, val pipeDelay: Int = 1) extends Module {
  require(maxDepth > 0, s"Depth must be > 0, got $maxDepth")

  val io = IO(new Bundle {
    val depth = Input(Valid(UInt(log2Ceil(maxDepth + 1).W)))
    val in    = Input(Valid(gen.cloneType))
    val out   = Output(Valid(gen.cloneType))
  })

  val depth = RegInit(maxDepth.U)
  when (io.depth.valid) {
    depth := io.depth.bits
  }

  val shr                                                = Reg(Vec(maxDepth - 1, gen.cloneType))
  val shrSelected: IndexedSeq[T] = shr.zipWithIndex.map { case (reg, idx) =>
    val included: Bool = (idx + 1).U < depth
    Mux(included, reg, 0.U.asTypeOf(reg)) //Ring[T].zero) //0.U.asTypeOf(reg))
  }

  val sum: T = // (Seq(io.in.bits) ++ shrSelected).reduce(_ + _)
    TreeReduce(Seq(io.in.bits) ++ shrSelected, (x:T,y:T) => x + y)
  io.out.bits := ShiftRegister(sum, pipeDelay)
  io.out.valid := ShiftRegister(io.in.fire(), pipeDelay, false.B, true.B)

  shr.scanLeft(io.in.bits) { case (in, out) =>
    when (io.in.fire()) {
      out := in
    }
    out
  }
}
