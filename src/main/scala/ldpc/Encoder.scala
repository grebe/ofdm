package ldpc

import chisel3._
import chisel3.util.Decoupled
import ofdm.TreeReduce

object SWEncoder {
  def apply(word: Seq[Boolean], params: LdpcParams): Seq[Boolean] = {
    require(word.length == params.generator.length)
    val selectedRows: Seq[Seq[Boolean]] = word.zip(params.generator).map({ case (w: Boolean, g: Seq[Boolean]) =>
      if (w) g else Seq.fill(g.length)(false) })
    selectedRows.foldLeft[Seq[Boolean]](Seq.fill(selectedRows.head.length)(false)) { case (prod, row) =>
        prod.zip(row).map({ case (l, r) => l ^ r})
    }
  }
}

class Encoder(params: LdpcParams) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(Vec(params.k, Bool()))))
  val out = IO(Decoupled(Vec(params.n, Bool())))

  val selectedRows = in.bits.zip(params.generator.toSeq).map({ case (i: Bool, p: Seq[Boolean]) =>
    p.map(pp => if (pp) Some(i) else None)
  })

  def rowSum(top: Seq[Option[Bool]], bottom: Seq[Option[Bool]]): Seq[Option[Bool]] = {
    top.zip(bottom).map({
      case (Some(t), Some(b)) => Some(t ^ b)
      case (Some(t), None) => Some(t)
      case (None, Some(b)) => Some(b)
      case _ => None
    })
  }

  out.bits := VecInit(TreeReduce(selectedRows, rowSum).flatten)

  out.valid := in.valid
  in.ready := out.ready
}
