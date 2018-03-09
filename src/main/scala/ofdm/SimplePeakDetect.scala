package ofdm

import chisel3._
import chisel3.util.Valid

class SimplePeakDetectIO(w: Int, maxNumPeaks: Int) extends Bundle {
  val in  = Input( Valid(UInt(w.W)))
  val numPeaks = Input(Valid(UInt(maxNumPeaks.W)))
  val peakDistance = Input(UInt(maxNumPeaks.W))
  val out = Output(Valid(UInt(w.W)))
}

class SimplePeakDetect(w: Int, maxNumPeaks: Int) extends Module {
  val io = IO(new SimplePeakDetectIO(w, maxNumPeaks))

  val peak = ShiftRegisterMem(io.in, maxNumPeaks, io.numPeaks)

  io.out.bits  := peak.bits
  io.out.valid := peak.valid && (io.in.bits - peak.bits) < io.peakDistance
}
