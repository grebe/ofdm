package motion

import freechips.rocketchip.config.Config
import midas.EndpointKey
import midas.widgets.EndpointMap

class WithMotionWidget extends Config((site, here, up) => {
  case EndpointKey => up(EndpointKey) ++ EndpointMap(Seq(new MotionEndpoint()))
})
