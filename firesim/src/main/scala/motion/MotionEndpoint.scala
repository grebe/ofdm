package motion

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.util.{Decoupled, Queue}
import freechips.rocketchip.config.Parameters
import midas.core.HostPort
import midas.widgets.{Endpoint, EndpointWidget, EndpointWidgetIO, Pulsify}

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

  val actuatorFifo = Module(new Queue(new MotionActuatorIO, 8))
  val sensorFifo = Module(new Queue(new MotionSensorIO, 8))

  val target = io.hPort.hBits
  val fire = io.hPort.toHost.hValid && io.hPort.fromHost.hReady && io.tReset.valid
  val targetReset = fire & io.tReset.bits

  io.hPort.toHost.hReady := io.hPort.fromHost.hReady
  io.hPort.fromHost.hValid := io.hPort.toHost.hValid
  io.tReset.ready := fire

  actuatorFifo.reset := reset.toBool || targetReset
  sensorFifo.reset := reset.toBool || targetReset

  actuatorFifo.io.enq.bits := target.actuator.bits
  actuatorFifo.io.enq.valid := target.actuator.valid
  target.actuator.ready := actuatorFifo.io.enq.ready

  target.sensor.bits := sensorFifo.io.deq.bits
  target.sensor.valid := sensorFifo.io.deq.valid
  sensorFifo.io.deq.ready := target.sensor.ready

  genROReg(actuatorFifo.io.deq.valid, "actuator_valid")
  genROReg(actuatorFifo.io.deq.bits.acceleration, "actuator_bits")
  Pulsify(genWOReg(actuatorFifo.io.deq.ready, "actuator_ready"), pulseLength = 1)

  Pulsify(genWOReg(sensorFifo.io.enq.valid, "sensor_valid"), pulseLength = 1)
  genWOReg(sensorFifo.io.enq.bits.frontDistance, "sensor_bits_frontDistance")
  genWOReg(sensorFifo.io.enq.bits.backDistance, "sensor_bits_backDistance")
  genWOReg(sensorFifo.io.enq.bits.velocity, "sensor_bits_velocity")
  genROReg(sensorFifo.io.enq.ready, "sensor_ready")

  genCRFile()
}
