package ofdm

import chisel3._
import chisel3.internal.requireIsHardware

object AdjustableShiftRegister {
  def apply[T <: Data](in: T, maxDepth: Int, depth: UInt, resetData: T, en: Bool = true.B): T = {
    require(maxDepth > 0)
    requireIsHardware(in)
    assert(depth <= maxDepth.U)

    val adjustableShiftRegister = Module(new chisel3.util.Queue(chiselTypeOf(in), entries = maxDepth, pipe = true, flow = true))

    adjustableShiftRegister.io.enq.bits := in
    adjustableShiftRegister.io.enq.valid := adjustableShiftRegister.io.count <= depth && en
    adjustableShiftRegister.io.deq.ready := adjustableShiftRegister.io.count >= depth && en

    Mux(adjustableShiftRegister.io.deq.fire(), adjustableShiftRegister.io.deq.bits, resetData)
  }
}
