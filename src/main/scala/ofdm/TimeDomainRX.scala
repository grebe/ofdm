package ofdm

import chisel3._
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
  autocorrParams: AutocorrParams[DspComplex[T]]
) {
  def toSyncParams() = SyncParams(protoIn, protoOut, maxNumPeaks, timeStampWidth, autocorrParams)
}

class TimeDomainRX[T <: Data : Real: BinaryRepresentation](params: RXParams[T]) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(params.protoIn)))
  val out = IO(Decoupled(StreamAndTime(params.toSyncParams())))

  val threshold: T = IO(Input(params.protoIn.real))

  val autocorrConfig   = IO(AutocorrConfigIO(params.autocorrParams))
  val peakDetectConfig = IO(SimplePeakDetectConfigIO(maxNumPeaks = params.maxNumPeaks))

  val en = IO(Input(Bool()))

  val globalCycleCounter = GlobalCycleCounter(64, "rx", enable = en && in.fire())

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

  // estimate CFO using CORDIC
  val cordic = IterativeCORDIC.circularVectoring(autocorr.io.out.bits.real.cloneType, params.protoAngle)
  cordic.io.in.bits.x := autocorr.io.out.bits.real
  cordic.io.in.bits.y := autocorr.io.out.bits.imag
  cordic.io.in.valid := autocorr.io.out.

  // correct CFO
}
