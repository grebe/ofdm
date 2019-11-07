package ofdm

import chisel3._
import chisel3.internal.requireIsChiselType
import chisel3.util.{Decoupled, Queue, Valid, log2Ceil}

class MutatorCommandDescriptor(val numPass: Int, val idWidth: Int = 8) extends Bundle {
  val `type` = UInt(2.W)
  val last = UInt(1.W)
  val length = UInt(log2Ceil(numPass + 1).W)
  val id = UInt(idWidth.W)
}

object MutatorCommandDescriptor {
  val typeDrop = 0.U
  val typePass = 1.U
  val typeWaitForPacket = 2.U
}

/**
  * Take an input stream and modify it. You can:
  * - Drop samples
  * - Assert TLAST
  * according to run-time configurable rules.
  */
class StreamMutator[T <: Data](proto: T, numCount: Int, queueDepth: Int, idWidth: Int = 8) extends MultiIOModule {
  requireIsChiselType(proto)

  val in = IO(Flipped(Decoupled(proto)))
  val out = IO(Decoupled(proto))
  val tlast = IO(Output(Bool()))

  val commandIn = IO(Flipped(Decoupled(new MutatorCommandDescriptor(numCount, idWidth = idWidth))))

  val packetDetect = IO(Input(Bool()))
  val currentId = IO(Output(Valid(UInt(idWidth.W))))

  val dropCounter = RegInit(0.U(log2Ceil(numCount + 1).W))
  val passCounter = RegInit(0.U(log2Ceil(numCount + 1).W))
  val waitingForPacket = RegInit(false.B)
  val useTlast = RegInit(false.B)
  val notCounting = dropCounter === 0.U && passCounter === 0.U
  val commandActive = !notCounting || waitingForPacket
  val id = RegInit(0.U(idWidth.W))
  currentId.valid := commandActive
  currentId.bits := id
  assert(dropCounter === 0.U || passCounter === 0.U)

  val commandQueue = Module(new Queue(new MutatorCommandDescriptor(numCount), entries = 8, pipe = true))

  commandQueue.io.enq <> commandIn

  commandQueue.io.deq.ready := !commandActive
  when (commandQueue.io.deq.fire()) {
    when (commandQueue.io.deq.bits.`type` === MutatorCommandDescriptor.typeDrop) {
      dropCounter := commandQueue.io.deq.bits.length
    }
    when (commandQueue.io.deq.bits.`type` === MutatorCommandDescriptor.typePass) {
      passCounter := commandQueue.io.deq.bits.length
    }
    when (commandQueue.io.deq.bits.`type` === MutatorCommandDescriptor.typeWaitForPacket) {
      waitingForPacket := true.B
    }
    useTlast := commandQueue.io.deq.bits.last
    id := commandQueue.io.deq.bits.id
  }

  out.bits := in.bits
  in.ready := dropCounter > 0.U || (out.ready && passCounter > 0.U) || waitingForPacket
  out.valid := in.valid && dropCounter === 0.U && (passCounter > 0.U) && !waitingForPacket
  when (in.fire() && dropCounter > 0.U) {
    dropCounter := dropCounter - 1.U
  }
  when (in.fire() && passCounter > 0.U) {
    passCounter := passCounter - 1.U
  }
  when (packetDetect) {
    waitingForPacket := false.B
  }
  tlast := useTlast && passCounter === 1.U
}
