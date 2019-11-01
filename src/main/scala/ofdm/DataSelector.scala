package ofdm

import chisel3._
import chisel3.util.Decoupled

class DataSelector[T <: Data](params: RXParams[T]) extends MultiIOModule {
  val in = Flipped(Decoupled(Vec(params.nFFT, params.protoChannelEst)))
  val out = Decoupled(Vec(params.nDataSubcarriers, params.protoChannelEst))

  var outIdx = 0
  for (inIdx <- 0 until params.nFFT) {
    // skip if input is a pilot
    if (!params.pilotPos.contains(inIdx)) {
      out.bits(outIdx) := in.bits(inIdx)
      outIdx += 1
    }
  }
  require(outIdx == params.nDataSubcarriers - 1, "Wrong number of subcarriers")

  out.valid := in.valid
  in.ready := out.ready
}
