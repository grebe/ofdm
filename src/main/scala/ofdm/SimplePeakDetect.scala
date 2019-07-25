package ofdm

import chisel3._
import chisel3.util.{Valid, log2Ceil}

case class SimplePeakDetectConfigIO(maxNumPeaks: Int) extends Bundle {
  val numPeaks = Input(UInt(log2Ceil(maxNumPeaks+1).W))
  val peakDistance = Input(UInt(log2Ceil(maxNumPeaks+1).W))

  override def cloneType: this.type = SimplePeakDetectConfigIO(maxNumPeaks).asInstanceOf[this.type]
}

class SimplePeak[T <: Data](proto: T) extends Bundle {
  val peak = Output(Bool())
  val time = Output(UInt(64.W))
  val data = Output(proto.cloneType)

  override def cloneType: this.type = new SimplePeak(proto).asInstanceOf[this.type]
}

class SimplePeakDetectIO[T <: Data](proto: T, maxNumPeaks: Int) extends Bundle {
  val in  = Input( Valid(new SimplePeak(proto)))
  val out = Output(Valid(new SimplePeak(proto)))

  val config = Input(SimplePeakDetectConfigIO(maxNumPeaks = maxNumPeaks))

  override def cloneType: this.type = new SimplePeakDetectIO(proto, maxNumPeaks).asInstanceOf[this.type]
}

/**
  * Check that number of correlation peaks (determined by io.in.bits.peak) is > numPeaks
  * and that the time between the peaks (determined by io.in.bits.time) is < peakDistance
  * @param w
  * @param maxNumPeaks
  */
class SimplePeakDetect[T <: Data](val proto: T, val maxNumPeaks: Int) extends Module {
  val io = IO(new SimplePeakDetectIO(proto, maxNumPeaks))

  val peakCnt = RegInit(0.U(log2Ceil(maxNumPeaks + 1).W))

  val peak: SimplePeak[T] = AdjustableShiftRegister(
    in = io.in.bits,
    maxDepth = maxNumPeaks,
    depth = io.config.peakDistance,
    resetData = 0.U.asTypeOf(io.in.bits),
    en = io.in.valid
  )

  val peakOut = io.in.valid && peak.peak
  val peakIn = io.in.valid && io.in.bits.peak

  when (peakIn && !peakOut) {
    peakCnt := Mux(peakCnt === (maxNumPeaks - 1).U, peakCnt, peakCnt + 1.U)
  } .elsewhen (!peakIn && peakOut) {
    peakCnt := Mux(peakCnt === 0.U, peakCnt, peakCnt - 1.U)
  }

  when (io.out.valid) {
    peakCnt := 0.U // start over after a packet detect
  }

  io.out.bits  := peak
  io.out.valid := io.in.valid && peakCnt >= io.config.numPeaks
}
