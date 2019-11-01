package ofdm

import chisel3._
import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream.{AXI4StreamIdentityNode, StreamingAXI4DMA}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._


trait SynchronizedCapture[D, U, EO, EI, B <: Data] extends DspBlock[D, U, EO, EI, B] with HasCSR {
  override val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val counter = RegInit(0.U(64.W))
    counter := counter + 1.U

    val en = RegInit(false.B)
    val going = RegInit(false.B)

    val flow = going || (counter === 0.U && en)
    when (flow) {
      going := true.B
    }

    for (((i, _), (o, _)) <- streamNode.in.zip(streamNode.out)) {
      o.bits := i.bits
      i.ready := o.ready && flow
      o.valid := i.valid && flow
    }
  }
}

class AXI4SynchronizedCapture(address: AddressSet, beatBytes: Int = 8)(implicit p: Parameters = Parameters.empty) extends SynchronizedCapture[
  AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle
  ] with AXI4DspBlock with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address, beatBytes = beatBytes))
}

class AXI4SynchronizedDMA(address: AddressSet, beatBytes: Int = 8) extends LazyModule()(Parameters.empty) with AXI4DspBlock {
  val cap = LazyModule(new AXI4SynchronizedCapture(address, beatBytes = beatBytes))
  val dma = LazyModule(new StreamingAXI4DMA())

  val mnode = AXI4MasterNode(Seq(AXI4MasterPortParameters(Seq(AXI4MasterParameters("SyncDMA")))))
  val snode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(Seq(AXI4SlaveParameters(
    address = Seq(address),
    supportsRead = TransferSizes(8, 64),
    supportsWrite = TransferSizes(8, 64)
  )), beatBytes = beatBytes)))

  snode := dma.axiNode
  // cap.mem.get := mnode

  val mem = Some(cap.mem.get)

  val streamNode =  dma.streamNode := cap.streamNode

  lazy val module = new LazyModuleImp(this)
}

object AXI4SynchronizedDMA extends App {
  val dma = LazyModule(new AXI4SynchronizedDMA(
    AddressSet(0x0, 0xffff),
    beatBytes = 8
  ) with AXI4StandaloneBlock)
  chisel3.Driver.execute(args, () => dma.module)
}