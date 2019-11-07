package ofdm

import chisel3._
import chisel3.util.{Queue, log2Ceil}
import dspblocks.{AXI4HasCSR, DspBlock, HasCSR, TLHasCSR}
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4EdgeParameters, AXI4MasterPortParameters, AXI4RegisterNode, AXI4SlavePortParameters}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamBundlePayload, AXI4StreamIdentityNode}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice, ValName}
import freechips.rocketchip.interrupts.{IntRange, IntSourceNode, IntSourceParameters, IntSourcePortParameters}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}
import freechips.rocketchip.tilelink.{TLBundle, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLManagerPortParameters, TLRegisterNode}

trait SkidBuffer[D, U, EO, EI, B <: Data] extends DspBlock[D, U, EO, EI, B] with HasCSR {
  def depth: Int
  def beatBytes: Int
  val streamNode = AXI4StreamIdentityNode()
  val intNode = IntSourceNode(Seq(IntSourcePortParameters(Seq(IntSourceParameters(IntRange(2))))))

  lazy val module = new LazyModuleImp(this) {
    val (in, inP) = streamNode.in.head
    val (out, _) = streamNode.out.head
    val (int, _) = intNode.out.head

    val watershed = RegInit(depth.U)
    val en = RegInit(false.B)
    val queueOverflowed = RegInit(false.B)
    val queueExceededWatershed = RegInit(false.B)
    val drainUpstreamWhenDisabled = RegInit(false.B)

    val queue = Module(new Queue(new AXI4StreamBundlePayload(inP.bundle), depth))
    queue.io.enq <> in
    out <> queue.io.deq

    when (!en) {
      in.ready := drainUpstreamWhenDisabled
      queue.io.enq.valid := false.B
      queue.io.deq.ready := true.B
      out.valid := false.B
    }

    when (!queue.io.enq.ready) {
      queueOverflowed := true.B
    }
    when (queue.io.count >= watershed) {
      queueExceededWatershed := true.B
    }

    int(0) := queue.io.count >= watershed
    int(1) := !queue.io.enq.ready

    regmap(
      beatBytes * 0 -> Seq(RegField(1, en)),
      beatBytes * 1 -> Seq(RegField(log2Ceil(depth + 1), watershed,
        RegFieldDesc("watershed", "threshold for when the queue getting full should cause an interrupt"))),
      beatBytes * 2 -> Seq(RegField.r(log2Ceil(depth + 1), queue.io.count,
        RegFieldDesc("count", "number of entries in queue"))),
      beatBytes * 3 -> Seq(RegField(1, queueOverflowed,
        RegFieldDesc("overflowed", "set to true if the queue ever overflowed (need to clear from SW)"))),
      beatBytes * 4 -> Seq(RegField(1, queueExceededWatershed,
        RegFieldDesc("overflowWatershed", "set to true if the queue ever overflowed the watershed (need to clear from SW)"))),
      beatBytes * 5 -> Seq(RegField(1, drainUpstreamWhenDisabled,
        RegFieldDesc("drainUpstream", "when set, drain upstream queue. otherwise backpressure"))),
    )
  }
}

class AXI4SkidBuffer(address: AddressSet, val depth: Int = 1024, val beatBytes: Int = 4)
  (implicit valName: ValName, p: Parameters = Parameters.empty) extends
  SkidBuffer[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle]
  with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address, beatBytes = beatBytes))
}

object AXI4SkidBuffer {
  def apply(address: AddressSet, depth: Int = 1024, beatBytes: Int = 4)(implicit valName: ValName) = {
    val buf: AXI4SkidBuffer = LazyModule(new AXI4SkidBuffer(address, depth, beatBytes))
    (buf.streamNode, buf.mem.get, buf.intNode)
  }
}

class TLSkidBuffer(address: AddressSet, val depth: Int = 1024, val beatBytes: Int = 4)
  (implicit valName: ValName, p: Parameters = Parameters.empty) extends
  SkidBuffer[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle] with TLHasCSR {
  val dev = new SimpleDevice("bwrc,skid", Seq("bwrc,skid"))
  val mem = Some(TLRegisterNode(Seq(address), dev, beatBytes = beatBytes))
}

object TLSkidBuffer {
  def apply(address: AddressSet, depth: Int = 1024, beatBytes: Int = 4)(implicit valName: ValName) = {
    val buf: TLSkidBuffer = LazyModule(new TLSkidBuffer(address, depth, beatBytes))
    (buf.streamNode, buf.mem.get, buf.intNode)
  }
}
