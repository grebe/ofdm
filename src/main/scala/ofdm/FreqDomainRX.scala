package ofdm

import chisel3._
import chisel3.util.{Decoupled, Queue}
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
  val eq = Module(new FlatPilotEstimator(params))
  eq.in <> fft.io.out
  eq.pilots.foreach { _ := DspComplex.wire(Ring[T].one, Ring[T].zero) }
  val dataSelector = Module(new DataSelector(params))
  dataSelector.in <> eq.out
  val demod = Module(new QPSKDemod(params))
  demod.in <> dataSelector.out

  val demodQueue = Module(new Queue(chiselTypeOf(demod.out.bits), 2))
  demodQueue.io.enq <> demod.out

  // TODO interleave?

  val deser = Module(new ofdm.fft.PacketDeserializer(ofdm.fft.PacketSerDesParams(
    proto = chiselTypeOf(demod.out.bits),
    ratio = 2
  )))
  deser.io.in <> demodQueue.io.deq
  val deserFlat: Seq[T] = deser.io.out.bits.flatten.flatten
}
