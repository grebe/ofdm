package ofdm

import breeze.numerics.sincpi
import chisel3._
import chisel3.util.{Decoupled, ShiftRegister, Valid, log2Ceil}
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

class FlatPilotEqualizer[T <: Data : Ring : ConvertableTo](val params: RXParams[T])
extends MultiIOModule {
  val in = IO(Flipped(Decoupled(Vec(params.nFFT, params.protoChannelEst))))
  val out = IO(Decoupled(Vec(params.nFFT, params.protoFFTOut)))

  val estimates: Seq[DspComplex[T]] = for (i <- params.pilotPos) yield {
    in.bits(i)
  }

  val multWire = Wire(Vec(params.nFFT, params.protoChannelEst))
  val addLatency = DspContext.current.numAddPipes
  val realMultLatency = DspContext.current.numMulPipes
  val complexMultLatency = DspContext.current.complexMulPipe
  val totalLatency = complexMultLatency + realMultLatency + addLatency

  // left edge - use the first est for every subcarrier
  for (i <- 0 until params.pilotPos.head) {
    multWire(i) := ShiftRegister(in.bits(i) context_* estimates.head, totalLatency - complexMultLatency)
  }
  // interpolate ests for the middle subcarriers
  for ((begin :: end :: Nil, pilotIdx) <- params.pilotPos.sliding(2).zipWithIndex) {
    val extent = end - begin
    val leftEst = in.bits(begin) // estimates(pilotIdx)
    val rightEst = in.bits(end) // estimates(pilotIdx + 1)
    for (i <- 1 until extent) {
      // val leftCoeff = ConvertableTo[T].fromDoubleWithFixedWidth(sincpi(i.toDouble / extent), params.protoFFTOut.real)
      // val rightCoeff = ConvertableTo[T].fromDoubleWithFixedWidth(sincpi(1 - i.toDouble / extent), params.protoFFTOut.real)
      // val left = DspComplex.wire(leftEst.real context_* leftCoeff, leftEst.imag context_* leftCoeff)
      // val right = DspComplex.wire(rightEst.real context_* rightCoeff, rightEst.imag context_* rightCoeff)
      // multWire(begin + i) := ShiftRegister(in.bits(begin + i), realMultLatency + addLatency) context_* (left context_+ right)
    }
  }
  for (i <- 0 until params.nFFT) {
    val rightEsts = params.pilotPos.filter(_ > i)
    if (rightEsts.nonEmpty) {
      val est = in.bits(rightEsts.head)
      multWire(i) := ShiftRegister(in.bits(i) context_* est, totalLatency - complexMultLatency)
    }
  }
  // right edge - use the last est for every subcarrier
  for (i <- params.pilotPos.last + 1 until params.nFFT) {
    multWire(i) := ShiftRegister(in.bits(i) context_* estimates.last, totalLatency - complexMultLatency)
  }
  // put zero through pilots
  for (p <- params.pilotPos) {
    multWire(p).real := Ring[T].zero
    multWire(p).imag := Ring[T].zero
  }

  Skid(totalLatency, in, out) := multWire
}
