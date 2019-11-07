package ofdm

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, log2Ceil}
import dspblocks.{AXI4HasCSR, DspBlock, HasCSR, TLHasCSR}
import dsptools.numbers._
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4EdgeParameters, AXI4MasterPortParameters, AXI4RegisterNode, AXI4SlavePortParameters}
import freechips.rocketchip.amba.axi4stream.AXI4StreamIdentityNode
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts.{IntRange, IntSourceNode, IntSourceParameters, IntSourcePortParameters}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}
import freechips.rocketchip.tilelink.{TLBundle, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLManagerPortParameters, TLRegisterNode}

abstract class TimeDomainRXBlock[T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data](params: RXParams[T])
  extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {
  override val streamNode = AXI4StreamIdentityNode()
  val intnode = IntSourceNode(Seq(IntSourcePortParameters(Seq(IntSourceParameters(IntRange(0, 1))))))
  val schedule = BundleBridgeSource[DecoupledIO[SchedulingDescriptor]](() => Decoupled(new SchedulingDescriptor(32, 1)))

  def beatBytes: Int

  lazy val module = new LazyModuleImp(this) {
    val rx = Module(new TimeDomainRX(params))
    rx.mutatorCommandIn.valid := false.B
    rx.mutatorCommandIn.bits := DontCare // TODO

    val (in, inP) = streamNode.in.head
    val (out, outP) = streamNode.out.head

    require(mem.isDefined)
    require(inP.bundle.hasData)
    require(!inP.bundle.hasKeep)
    require(!inP.bundle.hasStrb)
    require(inP.bundle.dataBits >= params.protoADC.getWidth)
    require(inP.bundle.dataBits >= params.protoFFTIn.getWidth)

    // make registers to save control stuff
    val angleWidth = params.protoAngle.getWidth
    val autocorrFieldWidth = log2Ceil(params.autocorrParams.maxApart + 1)
    val peakDetectFieldWidth = log2Ceil(params.maxNumPeaks + 1)
    val autocorrFF = RegInit(0.U(angleWidth.W))
    val peakThreshold = RegInit(0.U(angleWidth.W))
    val peakOffset = RegInit(0.U(angleWidth.W))
    val freqMultiplier = RegInit(0.U(angleWidth.W))
    val autocorrDepthApart = RegInit(0.U(autocorrFieldWidth.W))
    val autocorrDepthOverlap = RegInit(0.U(autocorrFieldWidth.W))
    val peakDetectNumPeaks = RegInit(0.U(peakDetectFieldWidth.W))
    val peakDetectPeakDistance = RegInit(0.U(peakDetectFieldWidth.W))
    val globalCycleEn = RegInit(false.B)

    // connect registers to controls
    rx.autocorrFF := autocorrFF.asTypeOf(params.protoAngle)
    rx.peakThreshold := peakThreshold.asTypeOf(params.protoAngle)
    rx.peakOffset := peakOffset.asTypeOf(params.protoAngle)
    rx.freqMultiplier := freqMultiplier.asTypeOf(params.protoAngle)
    rx.autocorrConfig.depthApart := autocorrDepthApart
    rx.autocorrConfig.depthOverlap := autocorrDepthOverlap
    rx.peakDetectConfig.numPeaks := peakDetectNumPeaks
    rx.peakDetectConfig.peakDistance := peakDetectPeakDistance
    rx.globalCycleEn := globalCycleEn

    // connect input
    rx.in.valid := in.valid
    rx.in.bits := in.bits.data.asTypeOf(params.protoADC)
    in.ready := rx.in.ready

    // connect output
    out.valid := rx.out.valid
    rx.out.ready := out.ready
    out.bits.data := rx.out.bits.asUInt
    out.bits.last := rx.tlast
    out.bits.id := 0.U
    out.bits.dest := 0.U

    // gather regfields
    val fields = Seq(
      // settable registers
      RegField(angleWidth, autocorrFF,
        RegFieldDesc(name = "autocorrFF", desc = "forgetting factor for autocorr")),
      RegField(angleWidth, peakThreshold,
        RegFieldDesc(name = "peakThreshold", desc = "multiplicative factor for packet detect")),
      RegField(angleWidth, peakOffset,
        RegFieldDesc(name = "peakOffset", desc = "additive factor for packet detect")),
      RegField(angleWidth, freqMultiplier,
        RegFieldDesc(name = "freqMultiplier", desc = "multiplicative factor applied to frequency estimate")),
      RegField(autocorrFieldWidth, autocorrDepthApart,
        RegFieldDesc(name = "autocorrDepthApart", desc = "depth apart in autocorrelator")),
      RegField(autocorrFieldWidth, autocorrDepthOverlap,
        RegFieldDesc(name = "autocorrDepthOverlap", desc = "size of window to sum over in autocorrelator")),
      RegField(peakDetectFieldWidth, peakDetectNumPeaks,
        RegFieldDesc(name = "peakDetectNumPeaks",
          desc = "number of peaks to find in window of size peak distance for a packet to be detected")),
      RegField(peakDetectFieldWidth, peakDetectPeakDistance,
        RegFieldDesc(name = "peakDetectPeakDistance", desc = "size of window to consider fo packet detect")),
      RegField(1, globalCycleEn,
        RegFieldDesc(name = "globalCycleEn", desc = "Enable for global cycle counter")),

      // read-only status
      RegField.r(32, rx.currentCycle,
        RegFieldDesc(name = "currentCycle", desc = "current cycle of global cycle counter")),
      RegField.r(angleWidth, rx.freqOut.asUInt,
        RegFieldDesc(name = "freqOut", desc = "current frequency estimate")),
      RegField.r(params.timeStampWidth, rx.packetDetects,
        RegFieldDesc(name = "packetDetects",
          desc = "timestamp of packet detect (warning- stalls if packetDetectsCount = 0)")),
      RegField.r(params.timeStampWidth, rx.packetDetectsCount,
        RegFieldDesc(name = "packetDetectsCount", desc = "number of packet detects waiting to be read")),
    )

    // connect register map
    regmap(
      fields.zipWithIndex.map({ case (f, i) =>
          i * beatBytes -> Seq(f)
      }): _*
    )

    // connect interrupts
    intnode.out.head._1 := VecInit(rx.packetDetects.valid)

    schedule.out.head._1.valid := rx.packetDetects.valid
    schedule.out.head._1.bits.relative := true.B
    schedule.out.head._1.bits.tl.time := 200.U
    schedule.out.head._1.bits.tl.length := 100.U
  }
}

class TLTimeDomainRXBlock[T <: Data : Real: BinaryRepresentation](params: RXParams[T], address: AddressSet, _beatBytes: Int = 4)
  extends TimeDomainRXBlock[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle](params) with TLHasCSR {
  val device = new SimpleDevice("timedomainrx", Seq("bwrc,timedomainrx"))

  def beatBytes: Int = _beatBytes
  val mem = Some(TLRegisterNode(address = Seq(address), device = device, beatBytes = beatBytes))
}

class AXI4TimeDomainRXBlock[T <: Data : Real: BinaryRepresentation](params: RXParams[T], address: AddressSet, _beatBytes: Int = 4)
  extends TimeDomainRXBlock[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params) with AXI4HasCSR {
  def beatBytes: Int = _beatBytes
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}
