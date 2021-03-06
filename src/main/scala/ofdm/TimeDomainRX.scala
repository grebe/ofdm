package ofdm

import chisel3._
import chisel3.internal.requireIsChiselType
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._

case class RXParams[T <: Data : Ring]
(protoADC: DspComplex[T], protoAngle: T, protoFFTIn: DspComplex[T], protoTwiddle: DspComplex[T],
 protoLLR: T, maxNumPeaks: Int, timeStampWidth: Int, autocorrParams: AutocorrParams[DspComplex[T]],
 ncoParams: NCOParams[T], pilotPos: Seq[Int] = Seq(4, 12, 20, 28, 36, 44, 52, 60),
 queueDepth: Int = (1 << 13) - 1, nFFT: Int = 64) {
  Seq(protoADC, protoAngle, protoFFTIn, protoTwiddle).foreach { case proto =>
    requireIsChiselType(proto)
    // this is mostly to require that widths are defined!
    require(proto.getWidth > 0)
  }
  val protoFFTOut: DspComplex[T] = protoFFTIn.real match {
    case p: FixedPoint =>
      val addedBits = (log2Ceil(nFFT) + 1) / 2
      val proto = FixedPoint((p.getWidth + addedBits).W, p.binaryPoint + ((addedBits + 1)/ 2))
      DspComplex(proto, proto).asInstanceOf[DspComplex[T]]
    case r: DspReal => protoFFTIn
  }
  val protoChannelInv: T = protoFFTOut.real match {
    case p: FixedPoint =>
      FixedPoint((p.getWidth * 2).W, p.binaryPoint).asInstanceOf[T]
    case r: DspReal => protoFFTOut.real
  }
  val protoChannelEst: DspComplex[T] = protoFFTOut.real match {
    case p: FixedPoint =>
      val proto = FixedPoint((p.getWidth + 4).W, (p.binaryPoint.get - 2).BP)
      DspComplex(proto, proto).asInstanceOf[DspComplex[T]]
    case r: DspReal => protoFFTOut
  }
  val nPilots = pilotPos.length
  val nDataSubcarriers = nFFT - nPilots

  def toSyncParams() = SyncParams(protoADC, protoFFTIn, maxNumPeaks, timeStampWidth, autocorrParams)
}

class TimeDomainRX[T <: Data : Real: BinaryRepresentation](params: RXParams[T], counterOpt: Option[GlobalCycleCounter] = None) extends MultiIOModule {
  // Streaming IOs
  val in = IO(Flipped(Decoupled(params.protoADC)))
  val out = IO(Decoupled(StreamAndTime(params.toSyncParams())))
  val tlast = IO(Output(Bool()))

  // Subblock configuration IO
  val autocorrFF = IO(Input(params.protoAngle)) // TODO better proto
  val peakThreshold: T = IO(Input(params.protoAngle))
  val peakOffset: T    = IO(Input(params.protoAngle))
  val freqMultiplier: T = IO(Input(params.protoAngle))
  val autocorrConfig   = IO(AutocorrConfigIO(params.autocorrParams))
  val peakDetectConfig = IO(SimplePeakDetectConfigIO(maxNumPeaks = params.maxNumPeaks))
  val inputDelay = IO(Flipped(Decoupled(UInt(32.W))))

  // Status stuff
  // val streamCount = IO(Output(UInt(log2Ceil(params.queueDepth).W)))
  val currentCycle = IO(Output(UInt(64.W)))
  val freqOut = IO(Output(params.protoAngle))
  val packetDetects = IO(Decoupled(UInt(params.timeStampWidth.W)))
  val packetDetectsCount = IO(Output(UInt(params.timeStampWidth.W)))

  val packetLength = IO(Input(UInt(params.timeStampWidth.W)))
  val samplesToDrop = IO(Input(UInt(params.timeStampWidth.W)))

  val globalCycleEn = IO(Input(Bool()))
  val globalCycleCounter = GlobalCycleCounter(64, "rx", enable = globalCycleEn && in.fire())

  currentCycle := globalCycleCounter.counter

  // detect packets with simple autocorrelation-based scheme
  val autocorr   = Module(new AutocorrSimple(params.autocorrParams))
  val peakDetect = Module(new SimplePeakDetect(params.protoFFTIn, params.maxNumPeaks))
  val segmenter = Module(new PacketSegmenter(chiselTypeOf(in.bits), 2000)) // TODO put in params
  val cpRemover = Module(new CPRemover(chiselTypeOf(in.bits), cpSize = 10, nFFT = params.nFFT))

  val peakDetectId = RegInit(0.U(8.W))

  autocorr.io.config <> autocorrConfig
  peakDetect.io.config <> peakDetectConfig

  autocorr.io.in.valid   := in.valid
  autocorr.io.in.bits    := in.bits

  val peakDetected =
    (autocorr.io.out.bits.abssq() > peakThreshold * autocorr.io.energy.bits + peakOffset)
  // autocorr.io.out.valid && autocorr.io.energy.valid // being accounted for in peakDetect valid signal

  peakDetect.io.in.valid := ShiftRegister(autocorr.io.out.valid && autocorr.io.energy.valid, 3,
    resetData = false.B, en = true.B)
  peakDetect.io.in.bits.peak := ShiftRegister(peakDetected, 3)
  peakDetect.io.in.bits.data := ShiftRegister(autocorr.io.out.bits, 3)
  peakDetect.io.in.bits.time := ShiftRegister(globalCycleCounter(), 3)

  val zero = Wire(DspComplex(Ring[T].zero, Ring[T].zero))
  zero.real := Ring[T].zero
  zero.imag := Ring[T].zero

  val accumAutocorr: DspComplex[T] = RegInit(t = params.protoFFTIn, init = zero)

  val packetDetectQueue = Module(new Queue(UInt(params.timeStampWidth.W), 16))
  when (peakDetect.io.out.fire()) {
    val mult = Wire(DspComplex(autocorrFF, autocorrFF))
    mult.real := autocorrFF
    mult.imag := Ring[T].zero
    accumAutocorr := Ring[DspComplex[T]].plus(mult * accumAutocorr, peakDetect.io.out.bits.data)
      // autocorr.io.out.bits
  }

  packetDetectQueue.io.enq.bits := peakDetect.io.out.bits.time
  packetDetectQueue.io.enq.valid := peakDetect.io.out.valid && false.B

  when (peakDetect.io.out.fire()) {
    peakDetectId := peakDetectId +& 1.U
  }

  assert(!peakDetect.io.out.valid || packetDetectQueue.io.enq.ready)
  packetDetects <> packetDetectQueue.io.deq
  packetDetectsCount := packetDetectQueue.io.count

  // estimate CFO using CORDIC
  val cordic = IterativeCORDIC.circularVectoring(accumAutocorr.real.cloneType, params.protoAngle)
  cordic.io.in.bits.x := accumAutocorr.real
  cordic.io.in.bits.y := accumAutocorr.imag
  cordic.io.in.bits.z := Ring[T].zero
  cordic.io.in.valid := RegNext(peakDetect.io.out.fire(), init = false.B)
  cordic.io.out.ready := true.B

  val freq = RegInit(t = params.protoAngle, init = Ring[T].zero)
  freqOut := freq

  when (cordic.io.out.fire()) {
    freq := cordic.io.out.bits.z
  }

  // correct CFO
  val nco = Module(new NCO(params.ncoParams))
  val ncoQueue = Module(new Queue(nco.io.out.bits.cloneType, 4, pipe=false, flow=false))
  ncoQueue.io.enq.bits := nco.io.out.bits
  ncoQueue.io.enq.valid := nco.io.out.valid

  nco.io.en := ncoQueue.io.enq.ready
  nco.io.freq := freq * freqMultiplier

  val maxInputDelay: Int = // autocorr delay
    params.autocorrParams.maxApart + params.autocorrParams.maxOverlap + params.autocorrParams.mulPipeDelay
  inputDelay.ready := true.B

  val inputQueue = Module(new Queue(chiselTypeOf(in.bits), 1, pipe = true, flow = true))
  val delayedInput = Module(new ShiftRegisterMem(chiselTypeOf(in.bits), maxDepth = maxInputDelay))
  val outputQueue = Module(new Queue(chiselTypeOf(in.bits), 1, pipe = true, flow = false))

  delayedInput.io.depth.valid := inputDelay.valid
  delayedInput.io.depth.bits := inputDelay.bits

  in.ready := inputQueue.io.enq.ready
  inputQueue.io.enq.valid := in.valid
  inputQueue.io.enq.bits := in.bits

  inputQueue.io.deq.ready := outputQueue.io.enq.ready
  delayedInput.io.in.valid := inputQueue.io.deq.valid && outputQueue.io.enq.ready
  delayedInput.io.in.bits := inputQueue.io.deq.bits

  outputQueue.io.enq.bits := delayedInput.io.out.bits
  outputQueue.io.enq.valid := delayedInput.io.out.valid

  ncoQueue.io.deq.ready := outputQueue.io.deq.fire()
  assert(outputQueue.io.deq.fire() === ncoQueue.io.deq.fire(),
    "delayed input should be modulated by valid nco output")

  segmenter.in.bits := outputQueue.io.deq.bits * ncoQueue.io.deq.bits
  segmenter.in.valid := outputQueue.io.deq.valid
  segmenter.packetLength := packetLength
  segmenter.samplesToDrop := samplesToDrop
  segmenter.packetDetect := peakDetect.io.out.fire()
  outputQueue.io.deq.ready := segmenter.in.ready

  cpRemover.in <> segmenter.out
  cpRemover.tlastIn := segmenter.tlast

  out.valid := cpRemover.out.valid
  cpRemover.out.ready := out.ready
  out.bits.stream := cpRemover.out.bits
  tlast := cpRemover.tlastOut
  out.bits.time := 0.U
}
