package ofdm

import chisel3._
import chisel3.core.requireIsChiselType
import chisel3.experimental.MultiIOModule
import chisel3.util._
import dsptools.numbers._

case class RXParams[T <: Data]
(
  protoIn: DspComplex[T],
  protoOut: DspComplex[T],
  protoAngle: T,
  maxNumPeaks: Int,
  timeStampWidth: Int,
  autocorrParams: AutocorrParams[DspComplex[T]],
  ncoParams: NCOParams[T],
  queueDepth: Int = (1 << 13) - 1
) {
  requireIsChiselType(protoIn)
  requireIsChiselType(protoOut)
  requireIsChiselType(protoAngle)
  def toSyncParams() = SyncParams(protoIn, protoOut, maxNumPeaks, timeStampWidth, autocorrParams)
}

class TimeDomainRX[T <: Data : Real: BinaryRepresentation](params: RXParams[T]) extends MultiIOModule {
  // Streaming IOs
  val in = IO(Flipped(Decoupled(params.protoIn)))
  val out = IO(Decoupled(StreamAndTime(params.toSyncParams())))
  val tlast = IO(Output(Bool()))

  // Subblock configuration IO
  val autocorrFF = IO(Input(params.protoAngle)) // TODO better proto
  val threshold: T = IO(Input(params.protoIn.real))
  val autocorrConfig   = IO(AutocorrConfigIO(params.autocorrParams))
  val peakDetectConfig = IO(SimplePeakDetectConfigIO(maxNumPeaks = params.maxNumPeaks))
  val mutatorCommandIn = IO(Flipped(Decoupled(new MutatorCommandDescriptor(params.queueDepth))))

  // Status stuff
  val streamCount = IO(Output(UInt(log2Ceil(params.queueDepth).W)))
  val currentCycle = IO(Output(UInt(64.W)))
  val freqOut = IO(Output(params.protoAngle))
  val packetDetects = IO(Decoupled(UInt(params.timeStampWidth.W)))
  val packetDetectsCount = IO(Output(UInt(params.timeStampWidth.W)))

  val globalCycleEn = IO(Input(Bool()))
  val globalCycleCounter = GlobalCycleCounter(64, "rx", enable = globalCycleEn && in.fire())

  currentCycle := globalCycleCounter.counter

  // detect packets with simple autocorrelation-based scheme
  val autocorr   = Module(new AutocorrSimple(params.autocorrParams))
  val peakDetect = Module(new SimplePeakDetect(params.timeStampWidth, params.maxNumPeaks))

  autocorr.io.config <> autocorrConfig
  peakDetect.io.config <> peakDetectConfig

  autocorr.io.in.valid   := in.valid
  autocorr.io.in.bits    := in.bits

  val peakDetected =
    (autocorr.io.out.bits.abssq() > threshold * autocorr.io.energy.bits.pow(2)) &&
      autocorr.io.out.valid && autocorr.io.energy.valid

  peakDetect.io.in.valid := peakDetected
  peakDetect.io.in.bits  := globalCycleCounter() + autocorr.totalDelay.U

  val zero = Wire(DspComplex(Ring[T].zero, Ring[T].zero))
  zero.real := Ring[T].zero
  zero.imag := Ring[T].zero

  val accumAutocorr: DspComplex[T] = RegInit(t = params.protoOut, init = zero)

  val packetDetectQueue = Module(new Queue(UInt(params.timeStampWidth.W), 16))
  when (peakDetect.io.out.fire()) {
    val mult = Wire(DspComplex(autocorrFF, autocorrFF))
    mult.real := autocorrFF
    mult.imag := Ring[T].zero
    accumAutocorr := mult * accumAutocorr + autocorr.io.out.bits // peakDetect.io.out.bits
  }

  packetDetectQueue.io.enq.bits := peakDetect.io.out.bits
  packetDetectQueue.io.enq.valid := peakDetect.io.out.valid
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

  nco.io.en := in.fire()
  nco.io.freq := freq

  val inputQueue = Module(new Queue(chiselTypeOf(in.bits), 1, pipe = true, flow = true))

  in.ready := inputQueue.io.enq.ready
  inputQueue.io.enq.valid := in.valid
  inputQueue.io.enq.bits := in.bits
  inputQueue.io.deq.ready := nco.io.out.valid

  val mutator = Module(new StreamMutator(chiselTypeOf(in.bits), params.queueDepth, params.queueDepth))
  mutator.commandIn <> mutatorCommandIn
  streamCount := mutator.streamCount
  tlast := mutator.tlast

  mutator.in.bits := inputQueue.io.deq.bits * nco.io.out.bits
  mutator.in.valid := inputQueue.io.deq.valid
  inputQueue.io.deq.ready := mutator.in.ready

  out.valid := mutator.out.valid
  mutator.out.ready := out.ready
  out.bits.stream := mutator.out.bits
  out.bits.time := 0.U
}
