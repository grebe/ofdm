package ofdm

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util.{Decoupled, log2Ceil}

class FFTBitRev[T <: Data](proto: T, nFFT: Int) extends MultiIOModule {
  require(nFFT > 0)
  require(((nFFT - 1) & nFFT) == 0, "nFFT must be power of 2")
  requireIsChiselType(proto)

  val in = IO(Flipped(Decoupled(Vec(nFFT, proto))))
  val out = IO(Decoupled(Vec(nFFT, proto)))

  out.valid := in.valid
  in.ready := out.ready

  for (i <- 0 until nFFT) {
    out.bits(i) := in.bits(BitRev(i, nBits=log2Ceil(nFFT)).toInt)
  }
}
