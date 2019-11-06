package ofdm

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dspblocks.DspBlock
import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class FreqDomainRXBlock[T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data](params: RXParams[T])
  extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {
  val mem = None
  val inWidthBytes = (params.protoFFTIn.getWidth + 7) / 8
  val outWidthBytes = (128 / 8)
  val streamNode = AXI4StreamAdapterNode(
    masterFn = { case m =>
        AXI4StreamMasterPortParameters(m.masterParams.copy(n = outWidthBytes))
    },
    slaveFn = { case s =>
        AXI4StreamSlavePortParameters(
          s.slaveParams.copy(hasData = true, hasStrb = false, hasKeep = false, alwaysReady = false))
    }
  )

  lazy val module = new LazyModuleImp(this) {
    val in = streamNode.in.head._1
    val (out, outP) = streamNode.out.head

    val freqRx = Module(new FreqDomainRX(params))
    freqRx.in.bits := in.bits.data.asTypeOf(params.protoFFTIn)
    freqRx.tlastIn := in.bits.last
    freqRx.in.valid := in.valid
    in.ready := freqRx.in.ready

    out.bits.data := freqRx.out.bits
    out.bits.dest := 0.U
    out.bits.id := 0.U
    out.bits.keep := BigInt("1" * outP.bundle.n).U
    out.bits.last := freqRx.tlastOut
    out.bits.strb := BigInt("1" * outP.bundle.n).U
    out.bits.user := 0.U
    out.valid := freqRx.out.valid
    freqRx.out.ready := out.ready
  }
}


class TLFreqDomainRXBlock[T <: Data : Real: BinaryRepresentation](params: RXParams[T]) // , address: AddressSet)
  extends FreqDomainRXBlock[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle](params) {
  // val device = new SimpleDevice("freqdomainrx", Seq("bwrc,freqdomainrx"))

  // def beatBytes: Int = mem.get.beatBytes
  // val mem = Some(TLRegisterNode(address = Seq(address), device = device))
}

class AXI4FreqDomainRXBlock[T <: Data : Real: BinaryRepresentation](params: RXParams[T]) // , address: AddressSet)
  extends FreqDomainRXBlock[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params) {
  // def beatBytes: Int = mem.get.beatBytes
  // val mem = Some(AXI4RegisterNode(address = address))
}
