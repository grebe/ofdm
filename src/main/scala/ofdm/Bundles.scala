package ofdm

import chisel3._
import chisel3.internal.requireIsChiselType

class SampleAndEstimate[T <: Data](protoSample: T, protoEstimate: T) extends Bundle {
  requireIsChiselType(protoSample)
  requireIsChiselType(protoEstimate)

  val sample = protoSample.cloneType
  val pilot = protoEstimate.cloneType
}
object SampleAndEstimate {
  def apply[T <: Data](protoSample: T, protoEstimate: T) =
    new SampleAndEstimate(protoSample = protoSample, protoEstimate = protoEstimate)
}

class SampleAndPilot[T <: Data](protoSample: T, protoPilot: T) extends Bundle {
  requireIsChiselType(protoSample)
  requireIsChiselType(protoPilot)

  val sample = protoSample.cloneType
  val pilot = protoPilot.cloneType

  override def cloneType: this.type = SampleAndPilot(protoSample, protoPilot).asInstanceOf[this.type]
}
object SampleAndPilot {
  def apply[T <: Data](protoSample: T, protoPilot: T) =
    new SampleAndPilot(protoSample = protoSample, protoPilot = protoPilot)
}

