package channel

import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream.{AXI4StreamBundle, AXI4StreamBundleParameters, AXI4StreamIdentityNode, AXI4StreamInwardNode, AXI4StreamMasterParameters, AXI4StreamOutwardNode, AXI4StreamSlaveParameters, AXI4StreamToBundleBridge, BundleBridgeToAXI4Stream}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, BundleBridgeSink, BundleBridgeSource, InModuleBody, LazyModule, LazyModuleImp, ValName}
import freechips.rocketchip.regmapper._
import ofdm.TreeReduce

abstract class SingleTapChannel[T <: Data : Ring : ConvertableTo, D, U, EO, EI, B <: Data]
(proto: T, nChannels: Int, beatBytes: Int = 4)
  extends LazyModule()(Parameters.empty) with HasRegMap {
  require(proto.isWidthKnown)

  val channels = for (_ <- 0 until nChannels) yield AXI4StreamIdentityNode()
  val interrupts = Vec(0, Bool())

  lazy val module = new LazyModuleImp(this) {
    val rawInputs = channels.map(_.in.head._1)
    val inputs = rawInputs.map(_.bits.data.asTypeOf(DspComplex(proto, proto)))
    val outputs: Seq[DspComplex[T]] = for ((i, c) <- rawInputs.zip(channels)) yield {
      val output = Wire(DspComplex(proto, proto))
      val rawOutput = c.out.head._1
      rawOutput.valid := i.valid
      i.ready := rawOutput.ready
      rawOutput.bits := i.bits
      rawOutput.bits.data := output.asUInt()
      output
    }
    val rawOutputs = channels.map(_.out.head._1)
    val uintTaps = Seq.fill(nChannels * nChannels) { RegInit(0.U(proto.getWidth)) }
    val taps = uintTaps.map(_.asTypeOf(DspComplex(proto, proto)))

    for ((o, taps) <- outputs.zip(taps.grouped(nChannels).toStream)) {
      val prods: Seq[DspComplex[T]] = inputs.zip(taps).map({ case ((i: DspComplex[T], t: DspComplex[T])) => i * t })
      o := TreeReduce(prods, (l: DspComplex[T], r: DspComplex[T]) => l + r)
    }

    regmap(uintTaps.zipWithIndex.map({ case (t, i) =>
      (i * beatBytes, Seq(RegField(beatBytes * 8, t)))
    }): _*)

  }
}

class AXI4SingleTapChannel[T <: Data : Ring : ConvertableTo]
(proto: T, nChannels: Int, address: AddressSet, concurrency: Int = 0, beatBytes: Int = 4) extends
  SingleTapChannel[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters,
    AXI4Bundle](proto, nChannels = nChannels, beatBytes = beatBytes) {
  val controlNode = AXI4RegisterNode(
    address = address,
    concurrency = concurrency,
    beatBytes = beatBytes,
    undefZero = true,
    executable = false)

  // Internally, this function should be used to populate the control port with registers
  def regmap(mapping: RegField.Map*) { controlNode.regmap(mapping:_*) }
}

object makeIO {
  private var inAXI4Cnt: Int = 0
  private var inAXI4StreamCnt: Int = 0
  private var outAXI4StreamCnt: Int = 0

  def inAXI4(node: AXI4InwardNode, params: AXI4BundleParameters = AXI4BundleParameters(addrBits = 16, dataBits = 32, idBits = 1)): Unit = {
    implicit val valName = ValName(s"memIn_$inAXI4Cnt")
    implicit val p: Parameters = Parameters.empty
    inAXI4Cnt += 1
    val inNode = BundleBridgeSource(() => new AXI4Bundle(params))
    node :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("in")))) :=
      inNode

    val memIn = InModuleBody { inNode.makeIO() }
  }
  def inAXI4Stream(node: AXI4StreamInwardNode, params: AXI4StreamBundleParameters = AXI4StreamBundleParameters(n = 4)): Unit = {
    implicit val valName = ValName(s"streamIn_$inAXI4StreamCnt")
    implicit val p: Parameters = Parameters.empty
    inAXI4StreamCnt += 1
    val inNode = BundleBridgeSource(() => new AXI4StreamBundle(params))
    node :=
      BundleBridgeToAXI4Stream(AXI4StreamMasterParameters()) :=
      inNode

    val streamIn = InModuleBody { inNode.makeIO() }
  }
  def outAXI4Stream(node: AXI4StreamOutwardNode): Unit = {
    implicit val valName = ValName(s"streamOut_$outAXI4StreamCnt")
    outAXI4StreamCnt += 1
    implicit val p: Parameters = Parameters.empty
    val outNode = BundleBridgeSink[AXI4StreamBundle]()
    outNode :=
      AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
      node

    InModuleBody { outNode.makeIO() }
  }
}

object SingleTapChannelApp extends App {
  val proto = FixedPoint(16.W, 15.BP)
  val lm = LazyModule(new AXI4SingleTapChannel(proto, 4, AddressSet(0, 0xFFFF)) {
    for (c <- channels) {
      makeIO.inAXI4Stream(c)
      makeIO.outAXI4Stream(c)
    }
    makeIO.inAXI4(controlNode)
  })
  chisel3.Driver.execute(args, () => lm.module)
}
