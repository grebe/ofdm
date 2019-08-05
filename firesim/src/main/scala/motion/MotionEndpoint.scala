package motion

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.util.Queue
import freechips.rocketchip.config.Parameters
import midas.core.HostPort
import midas.widgets.{Endpoint, EndpointWidget, EndpointWidgetIO}

class MotionEndpoint extends Endpoint {
  override def matchType(data: Data): Boolean = data match {
    case motionIO: MotionIO =>
      DataMirror.directionOf(motionIO.actuator.bits) == chisel3.experimental.Direction.Output
    case _ => false
  }

  override def widget(p: Parameters): EndpointWidget = {
    new MotionWidget()(p)
  }

  override def widgetName: String = "MotionWidget"
}

class MotionWidgetIO(p: Parameters) extends EndpointWidgetIO()(p) {
  val hPort = Flipped(HostPort(new MotionIO))
}

class MotionWidget()(p: Parameters) extends EndpointWidget()(p) {
  val io = IO(new MotionWidgetIO(p))

  val actuatorFifo = Module(new Queue(UInt(32.W), 8))
  val sensorFifo = Module(new Queue(UInt(96.W), 8))

  val target = io.hPort.hBits
  val fire = io.hPort.toHost.hValid && io.hPort.fromHost.hReady && io.tReset.valid & sensorFifo.io.enq.ready
  val targetReset = fire & io.tReset.bits

  io.hPort.toHost.hReady := fire
  io.hPort.fromHost.hValid := fire
  io.tReset.ready := fire

  actuatorFifo.reset := reset.toBool || targetReset
  sensorFifo.reset := reset.toBool || targetReset

  actuatorFifo.io.enq.bits := target.actuator.bits.acceleration
  actuatorFifo.io.enq.valid := target.actuator.valid
  target.actuator.ready := actuatorFifo.io.enq.ready

  target.sensor.bits := sensorFifo.io.deq.bits.asTypeOf(new MotionSensorIO)
  target.sensor.valid := sensorFifo.io.deq.valid
  sensorFifo.io.deq.ready := target.sensor.ready

  attachDecoupledSource(actuatorFifo.io.deq, "actuator")
  attachDecoupledSink(sensorFifo.io.enq, "sensor")

  genCRFile()
}
