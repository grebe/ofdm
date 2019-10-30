package ofdm

import chisel3._
import chisel3.util.{ShiftRegister, Valid}
import dsptools.numbers._

case class SyncParams[T <: Data]
(
  protoIn: DspComplex[T],
  protoOut: DspComplex[T],
  maxNumPeaks: Int,
  timeStampWidth: Int,
  autocorrParams: AutocorrParams[DspComplex[T]]
)

case class StreamAndTime[T <: Data](params: SyncParams[T]) extends Bundle {
  val stream = params.protoIn.cloneType
  val time   = UInt(params.timeStampWidth.W)

  override def cloneType: StreamAndTime.this.type = StreamAndTime(params).asInstanceOf[this.type]
}

case class SyncIO[T <: Data](params: SyncParams[T]) extends Bundle {
  val in  = Input (Valid(StreamAndTime(params)))
  val out = Output(Valid(StreamAndTime(params)))

  val threshold: T = Input(params.protoIn.real)

  val autocorrConfig   = AutocorrConfigIO(params.autocorrParams)
  val peakDetectConfig = SimplePeakDetectConfigIO(maxNumPeaks = params.maxNumPeaks)
}

class Sync[T <: Data : Real](params: SyncParams[T]) extends Module {
  val io: SyncIO[T] = IO(SyncIO(params))

  val autocorr   = Module(new AutocorrSimple(params.autocorrParams))
  val peakDetect = Module(new SimplePeakDetect(params.protoOut, params.maxNumPeaks))

  autocorr.io.config <> io.autocorrConfig
  peakDetect.io.config <> io.peakDetectConfig

  autocorr.io.in.valid   := io.in.valid
  autocorr.io.in.bits    := io.in.bits.stream

  val peakDetected =
    (autocorr.io.out.bits.abssq() > io.threshold * autocorr.io.energy.bits.pow(2)) &&
    autocorr.io.out.valid && autocorr.io.energy.valid

  peakDetect.io.in.valid := peakDetected
  peakDetect.io.in.bits  := ShiftRegister(io.in.bits.time, autocorr.totalDelay, en = io.in.valid)

  io.out.valid           := peakDetect.io.out.valid
  io.out.bits.time       := peakDetect.io.out.bits
  io.out.bits.stream     := autocorr.io.out.bits
}
