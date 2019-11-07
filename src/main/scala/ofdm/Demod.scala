package ofdm

import chisel3._
import chisel3.util.Decoupled
import dsptools.numbers._

class QPSKDemodUnit[T <: Data : Real](protoIn: DspComplex[T], protoOut: T) extends MultiIOModule {
  val in = IO(Input(protoIn))
  val out = IO(Output(Vec(2, protoOut)))

  out(0) := in.real
  out(1) := in.imag
}

class QPSKDemod[T <: Data : Real](params: RXParams[T]) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(Vec(params.nDataSubcarriers, params.protoChannelEst))))
  val out = IO(Decoupled(Vec(params.nDataSubcarriers, Vec(2, params.protoLLR))))

  in.bits.zip(out.bits).foreach { case (i, o) =>
      val demod = Module(new QPSKDemodUnit(params.protoChannelEst, params.protoLLR))
      demod.in := i
      o := demod.out
  }
  out.valid := in.valid
  in.ready := out.ready
}
