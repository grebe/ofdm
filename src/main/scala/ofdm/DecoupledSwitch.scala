package ofdm

import chisel3._
import chisel3.internal.{requireIsChiselType, requireIsHardware}
import chisel3.util.{Decoupled, DecoupledIO, log2Ceil}

class DecoupledSwitch[T <: Data](proto: T, n: Int) extends MultiIOModule {
  requireIsChiselType(proto)
  require(n > 0)

  val in = IO(Flipped(Decoupled(proto)))
  val out = IO(Vec(n, Decoupled(proto)))
  val sel = IO(Input(UInt(log2Ceil(n).W)))

  in.ready := false.B
  for (i <- 0 until n) {
    out(i).valid := false.B
    out(i).bits := in.bits
    when (sel === i.U) {
      in.ready := out(i).ready
      out(i).valid := in.valid
    }
  }
}

object DecoupledSwitch {
  def apply[T <: Data](in: T, sel: UInt, n: Int): Vec[DecoupledIO[T]] = {
    requireIsHardware(in)
    val switch = Module(new DecoupledSwitch(chiselTypeOf(in), n))
    switch.in <> in
    switch.sel := sel
    switch.out
  }
}
