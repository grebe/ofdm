package ofdm

import chisel3._
import chisel3.core.requireIsChiselType
import chisel3.experimental.MultiIOModule
import chisel3.util.{Decoupled, Queue, log2Ceil}

class MutatorCommandDescriptor(val numPass: Int) extends Bundle {
  val `type` = UInt(1.W)
  val last = UInt(1.W)
  val length = UInt(log2Ceil(numPass + 1).W)
}

object MutatorCommandDescriptor {
  val typeDrop = 0.U
  val typePass = 1.U
}

/**
  * Take an input stream and modify it. You can:
  * - Drop samples
  * - Assert TLAST
  * according to run-time configurable rules.
  */
class StreamMutator[T <: Data](proto: T, numCount: Int, queueDepth: Int) extends MultiIOModule {
  requireIsChiselType(proto)

  val in = IO(Flipped(Decoupled(proto)))
  val out = IO(Decoupled(proto))
  val tlast = IO(Output(Bool()))

  val commandIn = IO(Flipped(Decoupled(new MutatorCommandDescriptor(numCount))))

  val streamCount = IO(Output(UInt(log2Ceil(queueDepth).W)))

  val dropCounter = RegInit(0.U(log2Ceil(numCount + 1).W))
  val passCounter = RegInit(0.U(log2Ceil(numCount + 1).W))
  val useTlast = RegInit(false.B)
  val notCounting = dropCounter === 0.U && passCounter === 0.U
  assert(dropCounter === 0.U || passCounter === 0.U)

  val commandQueue = Module(new Queue(new MutatorCommandDescriptor(numCount), entries = 8, pipe = true))

  commandQueue.io.enq <> commandIn

  commandQueue.io.deq.ready := notCounting
  when (commandQueue.io.deq.fire()) {
    when (commandQueue.io.deq.bits.`type` === MutatorCommandDescriptor.typeDrop) {
      dropCounter := commandQueue.io.deq.bits.length
    }
    when (commandQueue.io.deq.bits.`type` === MutatorCommandDescriptor.typePass) {
      passCounter := commandQueue.io.deq.bits.length
    }
    useTlast := commandQueue.io.deq.bits.last
  }

  val queue = Module(new Queue(proto, queueDepth))

  streamCount := queue.io.count

  queue.io.enq <> in

  queue.io.deq.ready := !notCounting
  out.bits := queue.io.deq.bits
  out.valid := queue.io.deq.valid && passCounter =/= 0.U
  tlast := useTlast && passCounter === 1.U
}
