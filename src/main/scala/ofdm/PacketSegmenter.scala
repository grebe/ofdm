package ofdm

import chisel3._
import chisel3.util.{log2Ceil, Decoupled}
import chisel3.experimental.requireIsChiselType

class PacketSegmenter[T <: Data](proto: T, maxPacketLen: Int) extends MultiIOModule {
  requireIsChiselType(proto)
  val packetWidth = log2Ceil(maxPacketLen + 1)

  val in = IO(Flipped(Decoupled(proto)))
  val out = IO(Decoupled(proto))
  val tlast = IO(Output(Bool()))

  val packetLength = IO(Input(UInt(packetWidth.W)))
  val packetDetect = IO(Input(Bool()))
  val samplesToDrop = IO(Input(UInt(packetWidth.W)))

  val cnt = RegInit(0.U(packetWidth.W))
  val counting = RegInit(false.B)

  when (packetDetect) {
    counting := true.B
  }

  out.bits := in.bits
  out.valid := in.valid && counting && (cnt >= samplesToDrop)
  in.ready := out.ready || !counting

  when (counting && in.fire()) {
    cnt := cnt +% 1.U
  }
  tlast := false.B
  when (cnt === packetLength - 1.U) {
    cnt := 0.U
    tlast := true.B
    counting := false.B
  }

}
