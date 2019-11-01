package ofdm

import chisel3._
import chisel3.util.Decoupled
import dsptools.numbers._
import ofdm.fft.{DITDecimType, FFTParams, SDFFFT, SDFFFTDeserOut, SDFFFTType}

class FreqDomainRX[T <: Data : Real : BinaryRepresentation]
(
  params: RXParams[T],
  counterOpt: Option[GlobalCycleCounter] = None
) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(params.protoFFTIn)))
  val tlastIn = IO(Input(Bool()))
  val out = IO(Decoupled(params.protoFFTOut))
  val tlastOut = IO(Output(Bool()))

  val fftParams = ofdm.fft.FFTParams[T](
    numPoints = params.nFFT,
    protoIQ = params.protoFFTIn,
    protoTwiddle = params.protoTwiddle,
    fftType = SDFFFTType,
    decimType = DITDecimType,
    sdfRadix = 4,
    pipeline = true
  )
  val fft = Module(new SDFFFTDeserOut(fftParams))
  fft.io.in <> in
  val eq = Module(new FlatPilotEstimator(params, Seq(4, 12, 20, 28, 36, 44, 52, 60)))
  eq.in <> fft.io.out
  eq.pilots.foreach { _ := DspComplex.wire(Ring[T].one, Ring[T].zero) }

}
