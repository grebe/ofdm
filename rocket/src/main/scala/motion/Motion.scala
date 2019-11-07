package motion

import chisel3._
import chisel3.util.{Decoupled, Queue}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.regmapper.{HasRegMap, RegField, RegWriteFn}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink.{TLRegBundle, TLRegModule, TLRegisterRouter}

class MotionActuatorIO extends Bundle {
  val acceleration = UInt(32.W)
}

class MotionSensorIO extends Bundle {
  val frontDistance = UInt(32.W)
  val backDistance = UInt(32.W)
  val velocity = UInt(32.W)
}

class MotionIO extends Bundle {
  val actuator = Decoupled(new MotionActuatorIO)
  val sensor = Flipped(Decoupled(new MotionSensorIO))
}


/*
object MotionIO {
private var motions = Seq[MotionIO]()
def getSink(): MotionIO = {
  val motion = Wire(new MotionIO)
  motion := 0.U.asTypeOf(new MotionIO)

  BoringUtils.addSource(motion.actuator.bits, s"motion_actuator_bits_${motions.length}")
  BoringUtils.addSource(motion.actuator.valid, s"motion_actuator_valid_${motions.length}")
  BoringUtils.addSink(motion.actuator.ready, s"motion_actuator_ready_${motions.length}")
  BoringUtils.addSink(motion.sensor.bits, s"motion_sensor_bits_${motions.length}")
  BoringUtils.addSink(motion.sensor.valid, s"motion_sensor_valid_${motions.length}")
  BoringUtils.addSource(motion.sensor.ready, s"motion_sensor_ready_${motions.length}")

  motions +:= motion
  motion
}

def makeSources(): Seq[MotionIO] = {
  for ((m, i) <- motions.zipWithIndex) yield {
    val motion = Wire(new MotionIO)
    motion := 0.U.asTypeOf(new MotionIO)

    BoringUtils.addSink(motion.actuator.bits, s"motion_actuator_bits_$i")
    BoringUtils.addSink(motion.actuator.valid, s"motion_actuator_valid_$i")
    BoringUtils.addSource(motion.actuator.ready, s"motion_actuator_ready_$i")
    BoringUtils.addSource(motion.sensor.bits, s"motion_sensor_bits_$i")
    BoringUtils.addSource(motion.sensor.valid, s"motion_sensor_valid_$i")
    BoringUtils.addSink(motion.sensor.ready, s"motion_sensor_ready_$i")
    motion
  }
}
*/

trait HasMotionIO {
  val motion = new MotionIO
}

trait MotionModule extends HasRegMap {
  def beatBytes: Int

  val io: HasMotionIO

  val actuatorQueue = Module(new Queue(new MotionActuatorIO, 8))
  val sensorQueue = Module(new Queue(new MotionSensorIO, 8))

  actuatorQueue.io.deq <> io.motion.actuator
  sensorQueue.io.enq <> io.motion.sensor

  val acceleration = RegInit(0.U(32.W))

  actuatorQueue.io.enq.bits.acceleration := acceleration

  sensorQueue.io.deq.ready := false.B
  actuatorQueue.io.enq.valid := false.B

  regmap(
    0 * beatBytes -> Seq(RegField.r(32, actuatorQueue.io.count)),
    1 * beatBytes -> Seq(RegField(32, acceleration)),
    2 * beatBytes -> Seq(RegField.w(32, RegWriteFn((valid, _) => {
      actuatorQueue.io.enq.valid := valid
      actuatorQueue.io.enq.ready
    }))),
    3 * beatBytes -> Seq(RegField.r(32, sensorQueue.io.count)),
    4 * beatBytes -> Seq(RegField.r(32, sensorQueue.io.deq.bits.frontDistance)),
    5 * beatBytes -> Seq(RegField.r(32, sensorQueue.io.deq.bits.backDistance)),
    6 * beatBytes -> Seq(RegField.r(32, sensorQueue.io.deq.bits.velocity)),
    7 * beatBytes -> Seq(RegField.w(32, RegWriteFn((valid, _) => {
      sensorQueue.io.deq.ready := valid
      true.B
    }))),
  )

  // val interrupts = VecInit(sensorQueue.io.count > 0.U)
  interrupts := VecInit(sensorQueue.io.count > 0.U)
}

class TLMotionModule(address: BigInt, beatBytes: Int) extends
  TLRegisterRouter(address, "motion", Seq("bwrc,motion"), beatBytes = beatBytes, interrupts = 1)(
    new TLRegBundle(None, _) with HasMotionIO)(
    (x, y) => new TLRegModule(None, x, y) with MotionModule {
      def beatBytes = y.node.beatBytes
    }
  )(Parameters.empty)

trait HasMotion extends BaseSubsystem {
  val motion = LazyModule(new TLMotionModule(0x4000, 8))

  pbus.toVariableWidthSlave(Some("motion")) { motion.node }
  ibus.fromSync := motion.intnode
}