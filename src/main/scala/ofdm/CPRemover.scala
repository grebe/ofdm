package ofdm

import chisel3._
import chisel3.internal.requireIsChiselType
import chisel3.util.{Decoupled, log2Ceil}

class CPRemover[T <: Data](proto: T, cpSize: Int, nFFT: Int) extends MultiIOModule {
  requireIsChiselType(proto)

  val in = IO(Flipped(Decoupled(proto)))
  val out = IO(Decoupled(proto))
  val tlastIn = IO(Input(Bool()))
  val tlastOut = IO(Output(Bool()))

  val maxCnt = cpSize + nFFT

  val cnt = RegInit(0.U(log2Ceil(maxCnt + 1).W))

  when (in.fire()) {
    when (cnt === (maxCnt - 1).U || tlastIn) {
      cnt := 0.U
    } .otherwise {
      cnt := cnt +% 1.U
    }
  }

  out.bits := in.bits
  out.valid := in.valid && cnt >= cpSize.U // only pass input to output when counter is past the cyclic prefix
  in.ready := out.ready || cnt < cpSize.U // accept input if output is ready, or if it is the cyclic prefix (ok to discard)

  tlastOut := tlastIn || (cnt === (maxCnt - 1).U)
}
