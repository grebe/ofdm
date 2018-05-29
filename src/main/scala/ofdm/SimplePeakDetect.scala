package ofdm

import chisel3._
import chisel3.util.{Valid, log2Ceil}

case class SimplePeakDetectConfigIO(maxNumPeaks: Int) extends Bundle {
  val numPeaks = Input(Valid(UInt(log2Ceil(maxNumPeaks+1).W)))
  val peakDistance = Input(UInt(log2Ceil(maxNumPeaks+1).W))

  override def cloneType: this.type = SimplePeakDetectConfigIO(maxNumPeaks).asInstanceOf[this.type]
}

case class SimplePeakDetectIO(w: Int, maxNumPeaks: Int) extends Bundle {
  val in  = Input( Valid(UInt(w.W)))
  val out = Output(Valid(UInt(w.W)))

  val config = Input(SimplePeakDetectConfigIO(maxNumPeaks = maxNumPeaks))
}

class SimplePeakDetect(val w: Int, val maxNumPeaks: Int) extends Module {
  val io = IO(SimplePeakDetectIO(w, maxNumPeaks))

  val peak = ShiftRegisterMem(io.in, maxNumPeaks, io.config.numPeaks)

  io.out.bits  := peak.bits
  io.out.valid := peak.valid && (io.in.bits - peak.bits) < io.config.peakDistance
}
