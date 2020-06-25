package ofdm

import chisel3._
import chisel3.util._
import dsptools.numbers._

class OverlapSum[T <: Data : Ring](val gen: T, val maxDepth: Int) extends Module {
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

  val filledIdx = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  when (io.in.valid && filledIdx < maxDepth.U) {
    filledIdx := filledIdx +% 1.U
  }

  val shr = ShiftRegisterMem(io.in, maxDepth, io.depth)
  val inDelayed = RegNext(io.in.bits)
  val filled = filledIdx > depth - 1.U
  val filledDelayed = RegNext(filled)

  val sumT: T = (io.in.bits * log2Ceil(maxDepth)).cloneType
  val sum = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))
  when (shr.valid) {
    assert(RegNext(io.in.valid))
    when (filledDelayed) {
      sum := sum + inDelayed - shr.bits
    } .otherwise {
      sum := sum + inDelayed
    }
  }

  // restart when depth is changed
  when (io.depth.valid) {
    // TODO, this should perhaps use typeclasses, 0 is not always the additive identity
    sum := 0.U.asTypeOf(sum)
    filledIdx := 0.U
    filledDelayed := false.B // takes an extra cycle to be reset otherwise
  }

  io.out.bits := sum
  io.out.valid := RegNext(shr.fire() && filledDelayed, init=false.B)
}
