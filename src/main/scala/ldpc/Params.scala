package ldpc

import chisel3._

sealed trait Block {
  val blockSize: Int
  val degree: Seq[Int]
  def toBool(): Seq[Seq[Bool]]
  def toVariableSchedule(): Seq[VariableScheduleEntry]
}
case class ZeroBlock(blockSize: Int) extends Block {
  val degree = Seq.fill(blockSize)(0)
  def toVariableSchedule(): Seq[VariableScheduleEntry] = {
    Seq(VariableScheduleEntry(false, 0))
  }
  def toBool(): Seq[Seq[Bool]] = {
    Seq.fill(blockSize) { Seq.fill(blockSize) { false.B } }
  }
}
case class IdentityRightCircularShiftBlock(blockSize: Int, shift: Int) extends Block {
  require(shift >= 0 && shift < blockSize)

  val degree = Seq.fill(blockSize)(1)
  def toVariableSchedule(): Seq[VariableScheduleEntry] = {
    Seq(VariableScheduleEntry(true, shift))
  }

  def toBool(): Seq[Seq[Bool]] = {
    (for (i <- 0 until blockSize) yield {
      val idx = (i + shift) % blockSize
      Seq.fill(idx)(false.B) ++ Seq(true.B) ++ Seq.fill(blockSize - 1 - idx)(false.B)
    }).toSeq
  }
}

case class SumBlock(t0: Block, t1: Block) extends Block {
  require(t0.blockSize == t1.blockSize, "Can't add blocks with different sizes")
  val blockSize: Int = t0.blockSize
  val degree = t0.degree.zip(t1.degree).map { case (l, r) => l + r }

  def toVariableSchedule(): Seq[VariableScheduleEntry] = {
    t0.toVariableSchedule() ++ t1.toVariableSchedule()
  }
  def toBool(): Seq[Seq[Bool]] = {
    t0.toBool().zip(t1.toBool()).map({
      case (a: Seq[Bool], b: Seq[Bool]) =>
        a.zip(b).map({ case (aa: Bool, bb: Bool) => aa ^ bb })
    })
  }
}

case class VariableScheduleEntry(enabled: Boolean, shift: Int)
case class VariableSchedule(entries: Seq[(VariableScheduleEntry, Int)])

case class LdpcParams
(
  blockSize: Int,
  parity: Seq[Seq[Block]], // array of rows
  generator: Seq[Seq[Boolean]], //array of rows
  maxIters: Int = 6
) {
  parity.tail.foreach { p => require(p.length == parity.head.length, "all rows must have same length") }

  val n = blockSize * parity.head.length
  val k = blockSize * parity.length

  require(generator.length == k)
  generator.zipWithIndex.foreach { case (g, idx) =>
    require(g.length == n, s"Row $idx has length ${g.length}, expected $n") }

  val schedule: Seq[VariableSchedule] = {
    val unbalanced: Seq[Seq[Seq[VariableScheduleEntry]]] = parity.map(_.map(_.toVariableSchedule).toSeq).toSeq
    // attempt to rebalance: any extras get pushed into empty spaces and then get appended
    val primes: Seq[Seq[(VariableScheduleEntry, Int)]] = unbalanced.map(_.map(_.head).zipWithIndex)
    val extras: Seq[Seq[(VariableScheduleEntry, Int)]] = unbalanced.map(_.map(_.tail.zipWithIndex).reduce(_ ++ _))
    val extended = for ((p, e) <- primes.zip(extras)) yield {
      val extraIterator = e.iterator
      (for (pp <- p) yield {
        pp match {
          case v@(VariableScheduleEntry(true, _), _) => v
          case (VariableScheduleEntry(false, _), _) if extraIterator.hasNext => extraIterator.next()
          case x => x
        }}) ++ extraIterator
    }
    val maxInputs = extended.map(_.length).max
    val evenedOut = for (row <- extended) yield {
      row ++ Seq.fill(maxInputs - row.length)((VariableScheduleEntry(false, 0), 0))
    }
    evenedOut.map(row => VariableSchedule(row))
  }

  val columnDegree: Int = schedule.length
  val rowDegree: Int = schedule.head.entries.length

}

object Generator {
  private def toBoolSeq(b: Byte): Seq[Boolean] = {
    val seq = for (i <- 0 until 8) yield {
      (b & (1 << (7 - i))) != 0
    }
    seq
  }
  def fromHexString(blockSize: Int, n: Int, str: String): Seq[Seq[Boolean]] = {
    require(n % blockSize == 0)

    // divide by 2 b/c identity matrix is half
    val unshiftedRows = BigInt(str, 16).toByteArray.flatMap(toBoolSeq).grouped(n / 2).map(_.toSeq).toSeq

    // println(unshiftedRows.head.mkString(", "))

    def rotate(i: Int)(block: Seq[Boolean]): Seq[Boolean] = {
      block.drop(blockSize - i) ++ block.take(blockSize - i)
    }

    def expandRows(row: Seq[Boolean]) = {
      val blocks = row.grouped(blockSize).toSeq
      for (i <- 0 until blockSize) yield {
        blocks.flatMap(rotate(i))
      }
    }

    val w = unshiftedRows.flatMap(expandRows)

    val withIdentity = w.zipWithIndex.map { case (ww, i) =>
      (Seq.fill(i)(false) ++ Seq(true) ++ Seq.fill(n / 2 - i - 1)(false)) ++ ww
    }
    withIdentity
  }
}

object CCSDS {
  val bs64x128 = 16
  val params64x128 = LdpcParams(
    blockSize = bs64x128,
    parity = Seq(
      Seq(
        SumBlock(IdentityRightCircularShiftBlock(bs64x128, 7), IdentityRightCircularShiftBlock(bs64x128, 0)),
        IdentityRightCircularShiftBlock(bs64x128, 2),
        IdentityRightCircularShiftBlock(bs64x128, 14),
        IdentityRightCircularShiftBlock(bs64x128, 6),
        ZeroBlock(bs64x128),
        IdentityRightCircularShiftBlock(bs64x128, 0),
        IdentityRightCircularShiftBlock(bs64x128, 13),
        IdentityRightCircularShiftBlock(bs64x128, 0)
      ),
      Seq(
        IdentityRightCircularShiftBlock(bs64x128, 6),
        SumBlock(IdentityRightCircularShiftBlock(bs64x128, 15), IdentityRightCircularShiftBlock(bs64x128, 0)),
        IdentityRightCircularShiftBlock(bs64x128, 0),
        IdentityRightCircularShiftBlock(bs64x128, 1),
        IdentityRightCircularShiftBlock(bs64x128, 0),
        ZeroBlock(bs64x128),
        IdentityRightCircularShiftBlock(bs64x128, 0),
        IdentityRightCircularShiftBlock(bs64x128, 7)
      ),
      Seq(
        IdentityRightCircularShiftBlock(bs64x128, 4),
        IdentityRightCircularShiftBlock(bs64x128, 1),
        SumBlock(IdentityRightCircularShiftBlock(bs64x128, 15), IdentityRightCircularShiftBlock(bs64x128, 0)),
        IdentityRightCircularShiftBlock(bs64x128, 14),
        IdentityRightCircularShiftBlock(bs64x128, 11),
        IdentityRightCircularShiftBlock(bs64x128, 0),
        ZeroBlock(bs64x128),
        IdentityRightCircularShiftBlock(bs64x128, 3),
      ),
      Seq(
        IdentityRightCircularShiftBlock(bs64x128, 0),
        IdentityRightCircularShiftBlock(bs64x128, 1),
        IdentityRightCircularShiftBlock(bs64x128, 9),
        SumBlock(IdentityRightCircularShiftBlock(bs64x128, 13), IdentityRightCircularShiftBlock(bs64x128, 0)),
        IdentityRightCircularShiftBlock(bs64x128, 14),
        IdentityRightCircularShiftBlock(bs64x128, 1),
        IdentityRightCircularShiftBlock(bs64x128, 0),
        ZeroBlock(bs64x128)
      )
    ),
    generator = Generator.fromHexString(bs64x128, 128,
      "0E69166BEF4C0BC2" +
        "7766137EBB248418" +
        "C480FEB9CD53A713" +
        "4EAA22FA465EEA11"
    )
  )
}