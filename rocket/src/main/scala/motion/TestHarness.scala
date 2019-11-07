package motion

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.diplomacy.LazyModule

class TestHarness()(implicit p: Parameters) extends MultiIOModule {
  val users = 1

  val success = IO(Output(Bool()))

  val individual_successes = Wire(Vec(users, Bool()))
  success := individual_successes.reduce(_ | _)

  val duts = for (u <- 0 until users) yield {
    val dut = Module(LazyModule(new Top).module)
    dut.reset := reset.asBool | dut.debug.get.ndreset.asBool

    dut.dontTouchPorts()
    dut.tieOffInterrupts()
    dut.connectSimAXIMem()
    dut.connectSimAXIMMIO()
    dut.l2_frontend_bus_axi4.foreach(b => {
      b.tieoff()
      experimental.DataMirror.directionOf(b.ar.ready) match {
        case ActualDirection.Input =>
          b.r.bits  := DontCare
          b.b.bits  := DontCare
        case ActualDirection.Output =>
          b.ar.bits := DontCare
          b.aw.bits := DontCare
          b.w.bits  := DontCare
        case _ =>
      }
    })
    Debug.connectDebug(dut.debug, dut.psd, clock, reset.toBool(), out = individual_successes(u))

    dut
  }

  val motionIOs = duts.map(_.motion)

  val motionSimulator = Module(new MotionSimulator(users))

  motionSimulator.io.clock := clock
  motionSimulator.io.reset := reset

  motionSimulator.io.motion.zip(motionIOs).foreach { case (bb, io) => bb <> io }
}
