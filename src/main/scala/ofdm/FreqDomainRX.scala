package ofdm

import chisel3._
import chisel3.util.{Decoupled, Queue}
import dsptools.numbers._
import ldpc.{BPDecoder, CCSDS}
import ofdm.fft.{DITDecimType, PacketDeserializer, PacketSerDesParams, SDFFFTDeserOut, SDFFFTType}

class FreqDomainRX[T <: Data : Real : BinaryRepresentation]
(
  params: RXParams[T],
  counterOpt: Option[GlobalCycleCounter] = None
) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(params.protoFFTIn)))
  val tlastIn = IO(Input(Bool()))
  val out = IO(Decoupled(UInt(128.W)))
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
  val fft = Module(new R22SDF(fftParams.numPoints, protoIn = fftParams.protoIQ.real, protoOut = fftParams.protoIQ.real, protoTwiddle = fftParams.protoTwiddle.real))
  val fftDeser = Module(new PacketDeserializer(PacketSerDesParams(fftParams.protoIQ, fftParams.numPoints)))
  fft.in <> in
  fftDeser.io.in <> fft.out
  // val fft = Module(new SDFFFTDeserOut(fftParams))
  val eq = Module(new FlatPilotEstimator(params))
  eq.in <> fftDeser.io.out
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

  val ldpc = Module(new BPDecoder(params.protoLLR, CCSDS.params128x256))
  ldpc.in.bits.zip(deserFlat).foreach { case (l, d) =>
      l := d
  }
  ldpc.in.bits.drop(deserFlat.length).foreach { case (l) =>
    // no prior - these bits are removed
    l := Ring[T].zero
  }
  ldpc.in.valid := deser.io.out.valid
  deser.io.out.ready := ldpc.in.ready

  out.valid := ldpc.out.valid
  out.bits := ldpc.out.bits.asUInt
  ldpc.out.ready := out.ready

  tlastOut := false.B // TODO
}
