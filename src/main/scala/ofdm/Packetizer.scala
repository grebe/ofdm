package ofdm

import chisel3._
import chisel3.internal.requireIsChiselType
import chisel3.util.log2Ceil

case class PacketizerIO[T <: Data](proto: T, size: Int) extends Bundle {
  requireIsChiselType(proto)
  val in  = Flipped(IrrevocableLast(proto))
  val out = IrrevocableLast(proto)

  val includeMask = Input(Vec(size, Bool()))
}

class Packetizer[T <: Data](proto: T, size: Int) extends Module {
  val io = IO(PacketizerIO(proto, size))

  val count = RegInit(UInt(log2Ceil(size).W), 0.U)
  val nextCount = Wire(UInt())
  count := nextCount

  when (io.in.fire()) {
    nextCount := count + 1.U
    when (count + 1.U === size.U) {
      nextCount := 0.U
    }
  }
  when (io.in.fireLast()) {
    nextCount := 0.U
  }

  io.out.valid := io.in.fire() && io.includeMask(count)
  io.out.bits  := io.in.bits
  io.out.last  := nextCount === 0.U
}
