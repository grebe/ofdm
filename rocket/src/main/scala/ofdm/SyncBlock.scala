package ofdm

import chisel3._
import chisel3.core.requireIsChiselType
import dspblocks._
import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import spire.math
import spire.math.{Algebraic, Rational}

import scala.language.implicitConversions

object ScalarToComplexConvertableTo {
  implicit def scalarToComplex[T <: Data : Ring](c: ConvertableTo[T]): ConvertableTo[DspComplex[T]] =
    new ConvertableTo[DspComplex[T]] {
      def zero: T = c.fromDouble(0.0)
      override def fromAlgebraic(n: Algebraic): DspComplex[T] = DspComplex.wire(c.fromAlgebraic(n), zero)
      override def fromBigDecimal(n: BigDecimal): DspComplex[T] = DspComplex.wire(c.fromBigDecimal(n), zero)
      override def fromBigInt(n: BigInt): DspComplex[T] = DspComplex.wire(c.fromBigInt(n), zero)
      override def fromByte(n: Byte): DspComplex[T] = DspComplex.wire(c.fromByte(n), zero)
      override def fromDouble(d: Double, a: DspComplex[T]): DspComplex[T] = DspComplex.wire(c.fromDouble(d, a.real), c.fromDouble(0.0, a.imag))
      override def fromDouble(n: Double): DspComplex[T] = DspComplex.wire(c.fromDouble(n), zero)
      override def fromDoubleWithFixedWidth(d: Double, a: DspComplex[T]): DspComplex[T] =
        DspComplex.wire(c.fromDoubleWithFixedWidth(d, a.real), c.fromDoubleWithFixedWidth(0.0, a.imag))
      override def fromFloat(n: Float): DspComplex[T] = DspComplex.wire(c.fromFloat(n), zero)
      override def fromInt(n: Int): DspComplex[T] = DspComplex.wire(c.fromInt(n), zero)
      override def fromLong(l: Long): DspComplex[T] = DspComplex.wire(c.fromLong(l), zero)
      override def fromRational(n: Rational): DspComplex[T] = DspComplex.wire(c.fromRational(n), zero)
      override def fromReal(n: math.Real): DspComplex[T] = DspComplex.wire(c.fromReal(n), zero)
      override def fromShort(n: Short): DspComplex[T] = DspComplex.wire(c.fromShort(n), zero)
      override def fromType[B](b: B)(implicit evidence$1: ConvertableFrom[B]): DspComplex[T] =
        ???
        //DspComplex(c.fromType(b), zero)
    }
}

trait SyncBlock[T <: Data, D, U, EO, EI, B <: Data] extends DspBlock[D, U, EO, EI, B] {
  val streamNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters(numEndpoints = 1, alwaysReady = true))
  val beatBytes: Int
  val proto: T
  val maxNumPeaks: Int
  val autocorrParams: AutocorrParams[DspComplex[T]]
}

trait SyncBlockImp[T <: Data, D, U, EO, EI, B <: Data] extends LazyModuleImp with HasRegMap {
  def outer: SyncBlock[T, D, U, EO, EI, B] = wrapper.asInstanceOf[SyncBlock[T, D, U, EO, EI, B]]
  val proto: T = outer.proto
  val maxNumPeaks: Int = outer.maxNumPeaks
  val autocorrParams: AutocorrParams[DspComplex[T]] = outer.autocorrParams
  implicit def ev1: Real[T]
  implicit def ev3: ConvertableTo[DspComplex[T]] = ScalarToComplexConvertableTo.scalarToComplex(ev1)

  val streamNode: AXI4StreamSlaveNode = outer.streamNode
  val beatBytes : Int                 = outer.beatBytes

  require(streamNode.in.nonEmpty)
  requireIsChiselType(proto)

  //override val interrupts: Vec[Bool] = VecInit(Seq.fill(streamNode.in.length)(false.B))

  val (streamIns, streamInEdges) = streamNode.in.unzip
  streamInEdges.foreach { e =>
    require(e.bundle.u > 0, "Time must be encoded on the TUSER field but TUSER is 0 bits wide")
  }

  val streamRegMap: Seq[(Int, Seq[RegField])] = streamIns.zipWithIndex.flatMap { case (in, idx) =>
    // always ready
    in.ready := true.B

    val complexProto = DspComplex(proto, proto)
    //val matchedFilter = Module(new STF64MatchedFilter(proto, proto, proto))
    val autocorr = Module(new AutocorrSimple(autocorrParams))
    val peakDetect = Module(new SimplePeakDetect(in.params.u, maxNumPeaks))

    val threshold = RegInit(proto, ConvertableTo[T].fromDouble(0.5))
    val depthApart = RegInit(UInt(), 0.U)
    val depthOverlap = RegInit(UInt(), 0.U)
    val peakDistance = RegInit(UInt(), 0.U)
    val numPeaks = RegInit(UInt(), 0.U)

    val peak = RegInit(UInt(), 0.U)
    val corr = RegInit(complexProto, 0.U.asTypeOf(complexProto))

    autocorr.io.config.depthApart := depthApart
    autocorr.io.config.depthOverlap := depthOverlap

    peakDetect.io.peakDistance := peakDistance

    //matchedFilter.io.in.valid := in.valid
    //matchedFilter.io.in.bits := in.bits.data.asTypeOf(complexProto)

    autocorr.io.in.valid := in.valid
    autocorr.io.in.bits := in.bits.data.asTypeOf(complexProto)

    val peakDetected = (autocorr.io.out.bits.abssq() > threshold * autocorr.io.energy.bits) && autocorr.io.out.valid

    peakDetect.io.in.valid := in.fire() && peakDetected
    peakDetect.io.in.bits := in.bits.user

    when(peakDetect.io.out.valid) {
      interrupts(idx) := true.B
      peak := peakDetect.io.out.bits
      corr := autocorr.io.out.bits
    }

    val numFieldsPerStream = 7
    val beatBits = beatBytes * 8

    def idxToAddress(stream: Int, field: Int): Int = {
      require(field < numFieldsPerStream,
        s"field ($field) >= numFiledsPerStream ($numFieldsPerStream)- " +
          "did you add a field without updating numFieldsPerStream?")
      (stream * numFieldsPerStream + field) * beatBytes
    }

    // require(depthApart.getWidth <= beatBits)
    // require(depthOverlap.getWidth <= beatBits)
    // require(threshold.getWidth <= beatBits)

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
        peakDistance,
        RegFieldDesc(s"sync_$idx:peakDistance", "Distance between peaks in number of samples"))),
      idxToAddress(idx, 4) -> Seq(RegField(
        beatBits,
        RegReadFn(_ => {
          (true.B, numPeaks)
        }),
        RegWriteFn((b: Bool, u: UInt) => {
          peakDetect.io.numPeaks.valid := b
          peakDetect.io.numPeaks.bits  := u
          when (b) {
            numPeaks := u
          }
          true.B
        }),
        RegFieldDesc(s"sync_$idx:numPeaks", "Number of peaks in preamble"))),
      idxToAddress(idx, 5) -> Seq(RegField(
        beatBits,
        peak,
        RegFieldDesc(s"sync_$idx:peak", "timestamp of peak"))),
      idxToAddress(idx, 6) -> Seq(RegField(
        beatBits,
        RegReadFn(_ => {
          (true.B, corr.asUInt)
        }),
        RegWriteFn((b: Bool, u: UInt) => {
          when (b) {
            threshold := u.asTypeOf(proto)
          }; true.B
        }),
        RegFieldDesc(s"sync_$idx:corr", "output of autocorrelator corresponding to peak")))
    )
  }

  regmap(streamRegMap: _*)
}

class TLSyncBlock[T <: Data : Real]
(
  val proto: T,
  val maxNumPeaks: Int,
  val autocorrParams: AutocorrParams[DspComplex[T]],
  baseAddr: BigInt = 0, devname: String = "vsync", beatBytes: Int, concurrency: Int = 1
)(implicit p: Parameters)
  extends TLRegisterRouter(baseAddr, devname, Seq("ucb-bar,sync"), beatBytes = beatBytes, concurrency = concurrency, interrupts = 1)(
    new TLRegBundle(baseAddr, _))(
    new TLRegModule(baseAddr, _, _)
      with SyncBlockImp[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle] {
      override def ev1: Real[T] = implicitly[Real[T]]
    })
    with SyncBlock[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle] with TLDspBlock {
  val mem = Some(node)
}

class AXI4SyncBlock[T <: Data : Real]
(
  val proto: T,
  val maxNumPeaks: Int,
  val autocorrParams: AutocorrParams[DspComplex[T]],
  baseAddr: BigInt = 0, beatBytes: Int = 8, concurrency: Int = 1
)(implicit p: Parameters)
  extends AXI4RegisterRouter(baseAddr, beatBytes = beatBytes, concurrency = concurrency, interrupts = 1)(
    new AXI4RegBundle(baseAddr, _))(
    new AXI4RegModule(baseAddr, _, _)
      with SyncBlockImp[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle] {
      override def ev1: Real[T] = implicitly[Real[T]]
    })
  with SyncBlock[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle]
  with AXI4DspBlock {
  val mem = Some(node)
}
