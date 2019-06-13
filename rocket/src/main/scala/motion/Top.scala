package motion

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.system.{ExampleRocketSystem, ExampleRocketSystemModuleImp}

class Top(implicit params: Parameters) extends ExampleRocketSystem with HasMotion { outer =>
  override lazy val module = new ExampleRocketSystemModuleImp(this) {
    val motion = IO(new MotionIO)
    motion <> outer.motion.module.io.motion
  }
}
