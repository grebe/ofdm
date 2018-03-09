package ofdm

import chisel3._
import chisel3.core.requireIsChiselType
import dspblocks._
import dsptools.numbers._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

trait SyncBlock[T <: Data, D, U, EO, EI, B <: Data] extends DspBlock[D, U, EO, EI, B] {
  val streamNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters(numEndpoints = 0, alwaysReady = true))
  val beatBytes: Int
  val proto: T
  val maxNumPeaks: Int
  val autocorrParams: AutocorrParams[DspComplex[T]]
  implicit def ev1: Real[T] = implicitly[Real[T]]
  implicit def ev2: ConvertableTo[T] = implicitly[ConvertableTo[T]]
  implicit def ev3: ConvertableTo[DspComplex[T]] = implicitly[ConvertableTo[DspComplex[T]]]

}

trait SyncBlockImp[T <: Data, D, U, EO, EI, B <: Data] extends LazyModuleImp with HasRegMap {
  def outer: SyncBlock[T, D, U, EO, EI, B] = wrapper.asInstanceOf[SyncBlock[T, D, U, EO, EI, B]]
  val proto: T = outer.proto
  val maxNumPeaks: Int = outer.maxNumPeaks
  val autocorrParams: AutocorrParams[DspComplex[T]] = outer.autocorrParams
  implicit def ev1: Real[T] = outer.ev1
  implicit def ev2: ConvertableTo[T] = outer.ev2
  implicit def ev3: ConvertableTo[DspComplex[T]] = outer.ev3

  val streamNode: AXI4StreamSlaveNode = outer.streamNode
  val beatBytes : Int                 = outer.beatBytes

  require(streamNode.in.nonEmpty)
  requireIsChiselType(proto)

  override val interrupts: Vec[Bool] = VecInit(Seq.fill(streamNode.in.length)(false.B))

  val (streamIns, streamInEdges) = streamNode.in.unzip
  streamInEdges.foreach { e =>
    require(e.bundle.u > 0, "Time must be encoded on the TUSER field but TUSER is 0 bits wide")
  }

  val streamRegMap: Seq[(Int, Seq[RegField])] = streamIns.zipWithIndex.flatMap { case (in, idx) =>
    // always ready
    in.ready := true.B

    val complexProto = DspComplex(proto, proto)
    val matchedFilter = Module(new STF64MatchedFilter(complexProto, complexProto, complexProto))
    val autocorr = Module(new AutocorrSimple(autocorrParams))
    val peakDetect = Module(new SimplePeakDetect(in.params.u, maxNumPeaks))

    val threshold = RegInit(proto, ConvertableTo[T].fromDouble(0.5))
    val depthApart = RegInit(UInt(), 0.U)
    val depthOverlap = RegInit(UInt(), 0.U)

    val peak = RegInit(UInt(), 0.U)

    autocorr.io.config.depthApart := depthApart
    autocorr.io.config.depthOverlap := depthOverlap

    peakDetect.io.peakDistance := depthOverlap * 3.U
    peakDetect.io.numPeaks := (depthOverlap * 3.U) >> 1

    matchedFilter.io.in.valid := in.valid
    matchedFilter.io.in.bits := in.bits.asTypeOf(complexProto)

    autocorr.io.in.valid := in.valid
    autocorr.io.in.bits := in.bits.asTypeOf(complexProto)

    val peakDetected = autocorr.io.out.bits.abssq() > threshold * autocorr.io.energy.bits.asInstanceOf[T]

    peakDetect.io.in.valid := in.fire() && peakDetected
    peakDetect.io.in.bits := in.bits.user

    when(peakDetect.io.out.valid) {
      interrupts(idx) := true.B
      peak := peakDetect.io.out.bits
    }

    val numFieldsPerStream = 4
    val beatBits = beatBytes * 8

    def idxToAddress(stream: Int, field: Int): Int = {
      require(field < numFieldsPerStream,
        s"field ($field) >= numFiledsPerStream ($numFieldsPerStream)- " +
          "did you add a field without updating numFieldsPerStream?")
      (stream * numFieldsPerStream + field) * beatBytes
    }

    require(depthApart.getWidth <= beatBits)
    require(depthOverlap.getWidth <= beatBits)
    require(threshold.getWidth <= beatBits)

    Seq(
      idxToAddress(idx, 0) -> Seq(RegField(
        beatBits,
        depthApart,
        RegFieldDesc(s"sync_$idx:autocorr:depthApart", "Depth of shift register on autocorrelator delay path"))),
      idxToAddress(idx, 1) -> Seq(RegField(
        beatBits,
        depthOverlap,
        RegFieldDesc(s"sync_$idx:autocorr:depthOverlap", "Size of autocorrelator window"))),
      idxToAddress(idx, 2) -> Seq(RegField(
        beatBits,
        RegReadFn(_ => {
          (true.B, threshold.asUInt)
        }),
        RegWriteFn((b: Bool, u: UInt) => {
          when(b) {
            threshold := u.asTypeOf(proto)
          }; true.B
        }),
        RegFieldDesc(s"sync_$idx:threshold", "threshold for peak detection (0.5 by default)"))),
      idxToAddress(idx, 3) -> Seq(RegField(
        beatBits,
        peak,
        RegFieldDesc(s"sync_$idx:peak", "timestamp of peak")))
    )
  }

  regmap(streamRegMap: _*)
}

class TLSyncBlock[T <: Data : Ring : ConvertableTo]
(
  val proto: T,
  val maxNumPeaks: Int,
  val autocorrParams: AutocorrParams[DspComplex[T]],
  baseAddr: BigInt = 0, devname: String = "vsync", beatBytes: Int, concurrency: Int = 1
)(implicit p: Parameters, ev: ConvertableTo[DspComplex[T]])
  extends TLRegisterRouter(baseAddr, devname, Seq("ucb-bar,sync"), beatBytes = beatBytes, concurrency = concurrency)(
    new TLRegBundle(baseAddr, _))(
    new TLRegModule(baseAddr, _, _)
      with SyncBlockImp[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle])
    with SyncBlock[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle] with TLDspBlock {
  val mem = Some(node)
}