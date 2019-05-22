package ofdm

import freechips.rocketchip.amba.axi4.AXI4MasterModel
import breeze.math.Complex
import chisel3._
import chisel3.experimental._
import chisel3.iotesters.PeekPokeTester
import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream.{AXI4StreamFuzzer, AXI4StreamTransaction}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import ieee80211.IEEE80211
import org.scalatest.{FlatSpec, Matchers}

class AXI4SyncBlockTester[
// T <: Data,
M <: LazyModuleImp
//MultiIOModule with SyncBlockImp[
//  T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle
//  ]
](c: M)
  extends PeekPokeTester(c) with AXI4MasterModel {
  val outer = c.wrapper.asInstanceOf[SyncBlockTestModule[_]]
  override val memAXI: AXI4Bundle = outer.module.in
  val intbundles = outer.module.intout

  axiWriteWord(0,  64)
  axiWriteWord(8,  64)
  axiWriteWord(24, 160)
  axiWriteWord(32, 100)

  step(1000)
}

class AXI4MasterPort(params: AXI4MasterPortParameters)(implicit p: Parameters) extends LazyModule {
  val node = AXI4MasterNode(Seq(params))

  lazy val module = new LazyModuleImp(this) {
    val (ins,  inEdgeParams)  = node.in.unzip
    val (outs, outEdgeParams) = node.out.unzip

    ins.zip(outs).foreach { case (i, o) => o <> i }
  }
}

object AXI4MasterPort {
  def apply(params: AXI4MasterPortParameters =
            AXI4MasterPortParameters(Seq(AXI4MasterParameters("axi4master"))))
           (implicit p: Parameters, valName: ValName): AXI4MasterNode = {
    LazyModule(new AXI4MasterPort(params)).node
  }
}

class SyncBlockTestModule[T <: Data : Real]
(
  proto: T,
  maxNumPeaks: Int = 32,
  autocorrParams: AutocorrParams[DspComplex[T]],
  transactions: Seq[AXI4StreamTransaction] = AXI4StreamTransaction.linearSeq(100)
)(implicit p: Parameters) extends LazyModule {
  val sync = LazyModule(new AXI4SyncBlock(proto, maxNumPeaks = maxNumPeaks, autocorrParams = autocorrParams))
  val memMaster = AXI4MasterNode(Seq(AXI4MasterPortParameters(Seq(AXI4MasterParameters("axi4master")))))
  sync.mem.get := memMaster
  val fuzzer = AXI4StreamFuzzer(transactions = transactions, u = 64)
  sync.streamNode := fuzzer
  val intnode = IntSinkNode(IntSinkPortSimple(ports = 1, sinks = 1))
  intnode := sync.intnode

  lazy val module = new LazyModuleImp(this) {
    val in = IO(Flipped(AXI4Bundle(memMaster.out.head._1.params)))
    memMaster.out.head._1 <> in

    val intout = IO(Output(Vec(intnode.in.length, Bool())))
    intout := intnode.in.head._1

    // val ints = IO(Vec(sync.intnode.in.length, Bool()))
    // ints.zip(sync.intnode.in.map(_._1)).map(_ <> _)
  }
}

trait TestSyncBlock[T <: Data] extends AXI4SyncBlock[T] {
  def trans: Seq[AXI4StreamTransaction]
}

class SyncBlockSpec extends FlatSpec with Matchers {
  behavior of "Sync Block"

  implicit val p = Parameters.empty
  def axiDut[T <: Data : Real]
  (
    proto: T,
    maxNumPeaks: Int = 512,
    autocorrParams: AutocorrParams[DspComplex[T]],
    transactions: Seq[AXI4StreamTransaction] = AXI4StreamTransaction.linearSeq(100)
  )(implicit valName: ValName) = {
    LazyModule(new SyncBlockTestModule(
      proto, maxNumPeaks = maxNumPeaks, autocorrParams = autocorrParams, transactions = transactions
    )).module
  }

  it should "detect packets" in {
    val proto = FixedPoint(32.W, 14.BP)

    def unsignedBigInt(i: BigInt): BigInt = if (i < 0) {
      (BigInt(2) << proto.getWidth) + i
    } else {
      i
    }
    def complexToBigInt(c: Complex): BigInt = {
      val real = FixedPoint.toBigInt(c.real, binaryPoint = proto.binaryPoint.get - 2)
      val imag = FixedPoint.toBigInt(c.imag, binaryPoint = proto.binaryPoint.get - 2)
      (unsignedBigInt(real) << proto.getWidth) + unsignedBigInt(imag)
    }

    chisel3.iotesters.Driver.execute( Array("-fiwv", "-tbn", "verilator"),
      () => axiDut
        (
          proto,
          autocorrParams = AutocorrParams(DspComplex(proto),
            512,
            512),
          transactions = (IEEE80211.stf ++ Seq.fill(500)(Complex(0, 0)) ++ IEEE80211.stf ++ Seq.fill(500)(Complex(0, 0))).zipWithIndex.map { case (c, i) =>
              AXI4StreamTransaction(data = complexToBigInt(c), user = i)
          }
        )
    ) {
      c => new AXI4SyncBlockTester(c)
    } should be (true)
  }

}
