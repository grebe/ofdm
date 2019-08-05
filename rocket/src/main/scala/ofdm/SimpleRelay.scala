package ofdm

import chisel3._
import dsptools.numbers._
import freechips.rocketchip.amba.axi4.AXI4Xbar
import freechips.rocketchip.amba.axi4stream.{HasAXI4StreamCrossing, StreamingAXI4DMAWithCSR}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CrossingWrapper

/**
  * This is a simple relay. Its intended usage is to detect packets and then transmit a fixed message at a fixed
  * interval after the detected packet.
  */
class AXI4SimpleRelay[T <: Data : Real : BinaryRepresentation](rxParams: RXParams[T], baseAddress: BigInt = 0)
  extends LazyModule()(Parameters.empty) {

  val sAxiIsland = LazyModule(new CrossingWrapper(AsynchronousCrossing(safe=false)) with HasAXI4StreamCrossing)
  val sStreamIsland = LazyModule(new CrossingWrapper(AsynchronousCrossing(safe=false)) with HasAXI4StreamCrossing)

  val rx = sAxiIsland { LazyModule(new AXI4TimeDomainRXBlock(params = rxParams, address = AddressSet(baseAddress, 0xFF))) }
  val dma = sAxiIsland { LazyModule(new StreamingAXI4DMAWithCSR(csrAddress = AddressSet(baseAddress + 0x100, 0xFF))) }

  val aligner = sStreamIsland { LazyModule(new StreamAligner(addressSet = AddressSet(baseAddress + 0x200, 0xFF))) }
  val xbar = sAxiIsland { AXI4Xbar() }

  val scheduler = sAxiIsland { LazyModule(new AXI4_StreamScheduler(
    AddressSet(baseAddress + 0x300, 0xFF),
    beatBytes = 4,
    counterOpt = Some(rx.module.rx.globalCycleCounter)
  )) }

  sAxiIsland {
    rx.mem.get := xbar
    dma.axiSlaveNode := xbar
    aligner.axiNode := xbar
    scheduler.mem.get := xbar
    scheduler.hardCoded := rx.schedule
  }

  lazy val module = new LazyModuleImp(this)
}
