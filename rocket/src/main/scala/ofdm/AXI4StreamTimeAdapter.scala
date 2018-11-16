package ofdm

import chisel3._
import freechips.rocketchip.amba.axi4stream.AXI4StreamAdapterNode
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object AXI4StreamTimeAdapter {
  def apply(timeWidth: Int = 64)(implicit p: Parameters): (AXI4StreamAdapterNode, AXI4StreamAdapterNode) = {
    val timeAdapter = LazyModule(new AXI4StreamTimeAdapter(timeWidth))
    (timeAdapter.streamADCNode, timeAdapter.streamDACNode)
  }
}

class AXI4StreamTimeAdapter(val timeWidth: Int)(implicit p: Parameters) extends LazyModule {
  val streamADCNode: AXI4StreamAdapterNode = AXI4StreamAdapterNode(
    m => m.copy(m.masters.map(_.copy(u = timeWidth))),
    s => s
  )

  val streamDACNode: AXI4StreamAdapterNode = AXI4StreamAdapterNode(
    m => m.copy(m.masters.map(_.copy(u = 0))),
    s => s
  )

  lazy val module = new LazyModuleImp(this) {
    require(streamADCNode.in.length == streamADCNode.out.length,
      s"ADC should have same number of inputs and outputs (${streamADCNode.in.length} != ${streamADCNode.out.length}")
    require(streamDACNode.out.length == 1,
      "DAC should have one output")
    val (streamADCIns,  streamADCInEdges)  = streamADCNode.in.unzip
    val (streamADCOuts, streamADCOutEdges) = streamADCNode.out.unzip

    val (streamDACIns, streamDACInEdges) = streamDACNode.in.unzip
    val (streamDACOut, streamDACOutEdge) = streamDACNode.out.head

    val timeReg = RegInit(UInt(timeWidth.W), 0.U)
    timeReg := timeReg + 1.U

    val time = IO(Output(UInt()))
    time := timeReg

    streamADCIns.zip(streamADCOuts).foreach { case (in, out) =>
        out.bits      := in.bits
        out.bits.user := time
        out.valid     := in.valid
        in.ready      := out.ready
    }

    streamDACOut.valid := false.B
    streamDACIns.reverse.foreach { in =>
      in.ready := false.B
      val dacValid = in.valid && in.bits.user <= time
      when(dacValid) {
        streamDACOut.valid := true.B
        streamDACOut.bits := in.bits
        in.ready := streamDACOut.ready
      }
    }
  }
}
