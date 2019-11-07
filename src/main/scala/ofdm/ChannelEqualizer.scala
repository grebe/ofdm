package ofdm

import chisel3._
import chisel3.experimental.MultiIOModule
import chisel3.util.{Decoupled, log2Ceil}
import dsptools.DspContext
import dsptools.numbers._

class PreambleChannelEqualizer[T <: Data : Ring : ConvertableTo](params: RXParams[T]) extends MultiIOModule {
  val in = Flipped(Decoupled(params.protoFFTIn))
  val out = Decoupled(params.protoFFTOut)
  val in_last = Input(Bool())
  val out_last = Output(Bool())

  val pilots = IO(Input(Vec(params.nFFT, params.protoFFTOut)))
  val preambleRepetitions = IO(Input(UInt(32.W)))

  val weights = Reg(Vec(params.nFFT, params.protoChannelEst))
  val estimatingDone = RegInit(false.B)
  val subcarrierCount = Reg(UInt(log2Ceil(params.nFFT).W))
  val weightCount = Reg(UInt(log2Ceil(params.nFFT).W))
  when (in_last) {
    subcarrierCount := 0.U
    weightCount := 0.U
    for (w <- weights) {
      w := Ring[DspComplex[T]].zero
    }
  }
  when (in.fire()) {
    subcarrierCount := Mux(subcarrierCount === (params.nFFT - 1).U, 0.U, subcarrierCount +% 1.U)
  }


  val estimator = Module(new SinglePointChannelEstimator(params))

  estimator.in.valid := in.valid && !estimatingDone
  estimator.in.bits.sample := in.bits
  estimator.in.bits.pilot := weights(subcarrierCount)
  out.valid := in.valid && estimatingDone
  val skidIn = Wire(in.cloneType)
  skidIn.bits := in.bits
  skidIn.valid := in.valid
  in.ready := Mux(estimatingDone, skidIn.ready, estimator.in.ready)

  val skid = Skid(DspContext.current.numMulPipes, skidIn, out)
  skid := in.bits context_* weights(subcarrierCount)

  when (estimator.out.fire()) {
    weightCount := Mux(weightCount === (params.nFFT - 1).U, 0.U, weightCount +% 1.U)
    weights(weightCount) := estimator.out.bits
  }
}
