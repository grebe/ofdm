package ieee80211

import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.numerics.{cos, sin, sqrt}
import breeze.signal.iFourierTr

import scala.collection.mutable.ArrayBuffer

object IEEE80211 {
  val zero = Complex(0, 0)
  val one  = Complex(1, 0)
  val pp   = Complex(1, 1)
  val pm   = Complex(1, -1)
  val mp   = Complex(-1, 1)
  val mm   = Complex(-1, -1)

  // indexed from bin -26 -> bin 26
  val stfFreq = DenseVector(
    zero, zero, zero, zero, mm,   zero, zero, zero,
    mm,   zero, zero, zero, pp,   zero, zero, zero,
    pp,   zero, zero, zero, pp,   zero, zero, zero,
    pp,   zero, zero, zero, zero, zero, zero, zero,
    zero, zero, zero, zero, zero, zero, zero, zero,
    pp,   zero, zero, zero, mm,   zero, zero, zero,
    pp,   zero, zero, zero, mm,   zero, zero, zero,
    mm,   zero, zero, zero, pp,   zero, zero, zero
  ) map { x => sqrt(13.0 / 6.0) * x }
  val stf64 = iFourierTr(stfFreq)

  val stf = DenseVector.vertcat(stf64, stf64, stf64).toArray.slice(0, 160)

  val ltfFreq = DenseVector(
    zero,
    one, -one, -one,  one,  one, -one,  one, -one,
    one, -one, -one, -one, -one, -one, one,  one,
    -one, -one,  one, -one,  one, -one,  one,  one,
    one,  one,

    zero, zero, zero, zero, zero,
    zero, zero, zero, zero, zero, zero,

    one, one,
    -one, -one, one, one, -one, one, -one, one,
    one, one, one, one, one, -one, -one, one,
    one, -one, one, -one, one, one, one, one

  )

  val ltf64 = iFourierTr(ltfFreq)

  val ltf = DenseVector.vertcat(ltf64, ltf64, ltf64).toArray.slice(32,160+32)

  private def evenParity(x: Int): Boolean = {
    var p = x ^(x >> 1)
    p ^= (p >> 2)
    p ^= (p >> 4)
    p ^= (p >> 8)
    p ^= (p >> 16)
    (p & 1) == 0
  }

  def convStream(data: Array[Boolean], gen: BigInt): Array[Boolean] = {
    val stateBits = gen.bitLength
    data.sliding(stateBits).map { case d: Array[Boolean] =>
      var x = false
      for (i <- 0 until stateBits) yield {
        x ^= (((gen >> i) & 0x1) != 0) && d(i)
      }
      x
    }.toArray
  }
  def convEnc(data: Array[Boolean]): Array[Boolean] = {
    val g0 = BigInt("133", 8)
    val g1 = BigInt("171", 8)

    val out = ArrayBuffer[Boolean]()
    for ((a, b) <- convStream(data, g0) zip convStream(data, g1)) {
      out += a
      out += b
    }

    out.toArray
  }

  def convPuncture34(data: Array[Boolean]): Array[Boolean] = {
    data // TODO
  }

  def convPuncture23(data: Array[Boolean]): Array[Boolean] = {
    data // TODO
  }

  def interleaveP1(data: Array[Boolean]): Array[Boolean] = {
    require(data.length == 48)
    val grouped = data.grouped(3)
    (grouped.map(_.apply(0)) ++ grouped.map(_.apply(1)) ++ grouped.map(_.apply(2))).toArray
  }
  def interleaveP2(data: Array[Boolean], nbpsc: Int = 1): Array[Boolean] = {
    // val s = scala.math.max(nbpsc >> 1, 1)
    require(data.length == 48)
    data // TODO
  }
  def interleave(data: Array[Boolean], nbpsc: Int = 1): Array[Boolean] = {
    interleaveP2(interleaveP1(data), nbpsc)
  }

  def sig(rate: Int, length: Int, service: Int) = {
    require(rate >= 0 && rate <= 0xF)
    require(length >= 0 && length <= 0xFFF)
    require(service >= 0 && service <= 0x7F)

    val R = false
    val P = evenParity(rate | (length << 4))

    val bits = Array(
      // Rate
      (rate & 1) != 0, (rate & 2) != 0, (rate & 4) != 0, (rate & 8) != 0,
      // Reserved
      R,
      // Length
      (rate & 0x1) != 0, (rate & 0x2) != 0, (rate & 0x4) != 0, (rate & 0x8) != 0,
      (rate & 0x10) != 0, (rate & 0x20) != 0, (rate & 0x40) != 0, (rate & 0x80) != 0,
      (rate & 0x100) != 0, (rate & 0x200) != 0, (rate & 0x400) != 0, (rate & 0x800) != 0,
      // Parity
      P,
      // Tail
      false, false, false, false, false, false,
    )

    interleave(convEnc(bits), nbpsc = 1).map(if (_) one else zero)
  }

  def modSymbol(data: Array[Boolean]): Array[Complex] = {
    ???
  }

  def sigFreq(rate: Int, length: Int, service: Int) = {
    iFourierTr(DenseVector(sig(rate, length, service))).toArray
  }

  def addCFO(in: Seq[Complex], cfo: Double = 0.0, sampleRate: Double = 20.0e6): Seq[Complex] = {
    val coeff = 2 * math.Pi * cfo / sampleRate
    in.zipWithIndex.map { case (samp, idx) =>
      val rotation = Complex(cos(coeff * idx), sin(coeff * idx))
      rotation * samp
    }
  }

  def main(args: Array[String]): Unit = {
    println(stf64)
    println(s"Length is ${stf64.size}")

    println(s"LTF is $ltf64")
  }
}
