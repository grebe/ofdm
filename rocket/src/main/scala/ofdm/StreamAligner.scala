package ofdm

import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.amba.axi4.AXI4RegisterNode
import freechips.rocketchip.amba.axi4stream.AXI4StreamIdentityNode
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.RegField

/**
  * This block takes in a stream and passes it through to the other side.
  * It maintains a counter that wraps at a programmable value.
  * It selectively drops beats. When not enabled, incoming transactions are dropped.
  * When going from not enabled to enabled, it will drop incoming transactions until the counter returns back to zero,
  * upon which point it passes transactions to the output. From then on, the input passes to the output until enable is
  * changed to false.
  */
class StreamAligner
(
  maxCount: BigInt = 1L << 32,
  addressSet: AddressSet = AddressSet(0x0, 0xFFFF),
  beatBytes: Int = 4
) extends LazyModule()(Parameters.empty) {

  val streamNode = AXI4StreamIdentityNode()

  val axiNode = AXI4RegisterNode(address = addressSet, beatBytes = beatBytes)

  lazy val module = new LazyModuleImp(this) {
    val en = RegInit(false.B)
    val aligned = RegInit(false.B)
    val maxCountReg = RegInit(0.U(log2Ceil(maxCount).W))
    val cnt = RegInit(0.U(log2Ceil(maxCount).W))
    val cntPassthrough = RegInit(false.B)

    val enPrev = RegNext(en, false.B)

    when (streamNode.in.head._1.fire() || cntPassthrough) {
      when(cnt === maxCountReg - 1.U) {
        cnt := 0.U
      }.otherwise {
        cnt := cnt + 1.U
      }
    }

    (streamNode.in zip streamNode.out) foreach { case ((in, pIn), (out, pOut)) =>
      out.bits := in.bits
      when (cntPassthrough) {
        out.bits.data := cnt
      }
    }

    when (en && (cnt === 0.U || aligned)) {
        aligned := true.B

        (streamNode.in zip streamNode.out) foreach { case ((in, _), (out, _)) =>
          when (cntPassthrough) {
            out.valid := true.B
            in.ready := false.B
          } .otherwise {
            out.valid := in.valid
            in.ready := out.ready
          }
        }
    } .otherwise {
      aligned := false.B
      (streamNode.in zip streamNode.out) foreach { case ((in, _), (out, _)) =>
          out.valid := false.B
          in.ready := true.B
      }
    }

    axiNode.regmap(
      0 * beatBytes -> Seq(RegField(1, en)),
      1 * beatBytes -> Seq(RegField.r(1, aligned)),
      2 * beatBytes -> Seq(RegField.r(log2Ceil(maxCount), cnt)),
      3 * beatBytes -> Seq(RegField(log2Ceil(maxCount), maxCountReg)),
      4 * beatBytes -> Seq(RegField(1, cntPassthrough)),
    )
  }
}

object StreamAligner {
  def apply(
    maxCount: BigInt = 1L << 32,
    addressSet: AddressSet = AddressSet(0x0, 0xFFFF),
    beatBytes: Int = 4): (AXI4StreamIdentityNode, AXI4RegisterNode) = {
    val aligner = LazyModule(new StreamAligner(maxCount = maxCount, addressSet = addressSet, beatBytes = beatBytes))
    (aligner.streamNode, aligner.axiNode)
  }
}
