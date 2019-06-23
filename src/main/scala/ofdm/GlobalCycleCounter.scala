package ofdm

import chisel3._
import chisel3.util.experimental.BoringUtils

case class GlobalCycleCounter(val width: Int, val name: String, enable: Bool = true.B) {
  require(width >= 0)

  val counter = RegInit(0.U(width.W))
  when (enable) {
    counter := counter +& 1.U
  }
  BoringUtils.addSource(counter, s"GlobalCycleCounter_$name")

  def apply(): UInt = {
    val counter = Wire(UInt(width.W))
    counter := DontCare
    BoringUtils.addSink(counter, s"GlobalCycleCounter_$name")
    counter
  }
}
