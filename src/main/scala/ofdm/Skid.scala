package ofdm

import chisel3._
import chisel3.internal.requireIsHardware
import chisel3.util.{DecoupledIO, Queue, ShiftRegister, TransitName, log2Ceil}

object Skid {
  def apply[T <: Data](latency: Int, in: DecoupledIO[_ <: Data], out: DecoupledIO[T], en: Bool = true.B): T = {
    requireIsHardware(in)
    requireIsHardware(out)

    require(latency >= 0)
    if (latency == 0) {
      in.ready := out.ready
      out.valid := in.valid

      return out.bits
    }

    val queue = Module(new Queue(chiselTypeOf(out.bits), latency, pipe = latency == 1))

    val queueCounter = RegInit(0.U(log2Ceil(latency + 1).W))
    queueCounter := queueCounter +& in.fire() -& out.fire()

    queue.io.enq.valid := ShiftRegister(in.fire(), latency, resetData = false.B, en = en)
    assert(!queue.io.enq.valid || queue.io.enq.ready) // we control in.ready such that the queue can't fill up!

    in.ready := queueCounter < latency.U
    queue.io.deq.ready := out.ready
    out.valid := queue.io.deq.valid
    out.bits := queue.io.deq.bits

    // TransitName(out, queueCounter)
    TransitName(queue.io.enq.bits, out)
  }

}
