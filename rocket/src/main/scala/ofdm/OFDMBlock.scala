package ofdm

import chisel3._
import chisel3.internal.requireIsChiselType
import dspblocks._
import dsptools.numbers._
import freechips.rocketchip.amba.axi4stream.{AXI4StreamIdentityNode, SimpleSplitter}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._

trait OFDMBlock[T <: Data, D, U, EO, EI, B <: Data] extends DspBlock[D, U, EO, EI, B] {
  val streamNode = AXI4StreamIdentityNode()

  val proto: T
  val beatBytes: Int
  implicit def ev1: Real[T] = implicitly[Real[T]]
  implicit def ev2: ConvertableTo[T] = implicitly[ConvertableTo[T]]
  implicit def ev3: ConvertableTo[DspComplex[T]] = implicitly[ConvertableTo[DspComplex[T]]]

  requireIsChiselType(proto)
  require(mem.nonEmpty)

  val sync: SyncBlock[T, D, U, EO, EI, B]
  val adcRegister: DspRegister[D, U, EO, EI, B]
  val dacRegister: DspRegister[D, U, EO, EI, B]
  val queue: DspQueue[D, U, EO, EI, B]

  val (timeAdapterADC, timeAdapterDAC) = AXI4StreamTimeAdapter(beatBytes * 8)
  val splitter = SimpleSplitter()

  splitter               := timeAdapterADC   := streamNode
  sync.streamNode        := splitter
  adcRegister.streamNode := queue.streamNode := splitter
  streamNode             := dacRegister.streamNode

  // connect mem interfaces to bus
  Seq(sync.mem, adcRegister.mem, dacRegister.mem, queue.mem).foreach( _.map(mem.get := _))
}

class TLOFDMBlock[T <: Data]
(
  val proto: T,
  val baseAddr: BigInt = 0,
  val maxNumPeaks: Int = 40,
  val regSize: Int = 2048,
  val autocorrParams: AutocorrParams[DspComplex[T]],
  beatBytes: Int = 8,
  devname: String = "vofdm",
  concurrency: Int = 1
)(implicit p: Parameters, ev: ConvertableTo[DspComplex[T]]) extends TLRegisterRouter(baseAddr, devname, Seq("ucb-bar,ofdm"), concurrency = concurrency, beatBytes=beatBytes)(
  new TLRegBundle(baseAddr, _))(
  new TLRegModule(baseAddr, _, _))
  with OFDMBlock[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle] {
  val mem = Some(node)

  val sync = new TLSyncBlock(proto, maxNumPeaks, autocorrParams, beatBytes = beatBytes, baseAddr = baseAddr)
  val adcRegister = new TLDspRegister(regSize)
  val dacRegister = new TLDspRegister(regSize)
  val queue = new TLDspQueue(regSize)
}