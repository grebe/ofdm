package ofdm

import breeze.math.Complex
import breeze.signal.fourierTr
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.log2Ceil
import dsptools.{DspContext, DspTester}
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class BF2ITester[T <: Data](dut: BF2I[T]) extends DspTester(dut) {
  require(DspContext.current.numAddPipes == 0)

  poke(dut.en, false)
  step(5)

  poke(dut.sel, 0)
  for (i <- 0 until dut.delay) {
    poke(dut.en, false)
    val delay = Random.nextInt(5)
    step(delay)
    poke(dut.en, true)
    poke(dut.in, Complex(i, -i))
    step(1)
  }

  poke(dut.sel, 1)
  for (i <- 0 until dut.delay) {
    poke(dut.en, false)
    val delay = Random.nextInt(5)
    step(delay)
    poke(dut.en, true)
    poke(dut.in, Complex(2 * i, -2 * i))
    expect(dut.out, Complex(3 * i, -3 * i))
    step(1)
  }

  poke(dut.sel, 0)
  for (i <- 0 until dut.delay) {
    poke(dut.en, false)
    val delay = Random.nextInt(5)
    step(delay)
    poke(dut.en, true)
    poke(dut.in, Complex(0, 0))
    expect(dut.out, Complex(-i, i))
    step(1)
  }
}

object BF2ITester {
  def apply[T <: Data : Ring](proto: T, delay: Int): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String]("-tiwv"),
      () => new BF2I(proto, delay)) { c =>
      new BF2ITester(c)
    }
  }
}

class BF2IITester[T <: Data](dut: BF2II[T]) extends DspTester(dut) {
  require(DspContext.current.numAddPipes == 0)

  poke(dut.en, false)
  step(5)

  poke(dut.sel, false)
  poke(dut.inMultByJ, false)
  for (i <- 0 until dut.delay) {
    poke(dut.en, false)
    val delay = Random.nextInt(5)
    step(delay)
    poke(dut.en, true)
    poke(dut.in, Complex(i, -i))
    step(1)
  }

  poke(dut.sel, true)
  poke(dut.inMultByJ, false)
  for (i <- 0 until dut.delay) {
    poke(dut.en, false)
    val delay = Random.nextInt(5)
    step(delay)
    poke(dut.en, true)
    poke(dut.in, Complex(2 * i, -2 * i))
    expect(dut.out, Complex(3 * i, -3 * i))
    step(1)
  }

  poke(dut.sel, true)
  poke(dut.inMultByJ, true)
  for (i <- 0 until dut.delay) {
    poke(dut.en, false)
    val delay = Random.nextInt(5)
    step(delay)
    poke(dut.en, true)
    poke(dut.in, Complex(2 * i, -2 * i))
    expect(dut.out, Complex(-3 * i, -i))
    step(1)
  }

  poke(dut.sel, false)
  poke(dut.inMultByJ, false)
  for (i <- 0 until dut.delay) {
    poke(dut.en, false)
    val delay = Random.nextInt(5)
    step(delay)
    poke(dut.en, true)
    poke(dut.in, Complex(0, 0))
    expect(dut.out, Complex(i, 3 * i))
    step(1)
  }
}

object BF2IITester {
  def apply[T <: Data : Ring](proto: T, delay: Int): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String]("-tiwv"),
      () => new BF2II(proto, delay)) { c =>
      new BF2IITester(c)
    }
  }
}

class R22StageTester[T <: Data](dut: R22Stage[T]) extends DspTester(dut) {
  val outdelay = 2 * DspContext.current.numAddPipes

  val o0 = for (i <- 0 until dut.n / 4) yield {
    val offset = (3 * dut.n) / 2
    Complex(4 * i + offset, -4 * i - offset)
  }
  val o1 = for (i <- 0 until dut.n / 4) yield {
    val offset = -dut.n / 2
    Complex(offset, -offset)
  }
  val o2 = for (i <- 0 until dut.n / 4) yield {
    val offset = Complex(-dut.n / 2, dut.n / 2)
    offset
  }
  val o3 = for (i <- 0 until dut.n / 4) yield {
    val offset = Complex(-dut.n / 2, dut.n / 2)
    offset
  }
  val expectedOutput: Seq[Complex] = (Seq.fill(3 * dut.n / 4)(Complex(0, 0)) ++ o0 ++ o1 ++ o2 ++ o3)

  var inCnt: Int = 0
  var randCnt: Int = 0
  var inToOutCnt: Int = 0
  var outCnt: Int = 0

  while (inCnt < dut.n && outCnt < expectedOutput.length) {
    if (randCnt <= 0) {
      poke(dut.en, 1)
      poke(dut.in, Complex(inCnt, -inCnt))
      val ctrl = (inCnt >> ( log2Ceil(dut.n) - 2 )) & 0x3
      poke(dut.ctrl, ctrl)
      inCnt += 1
      inToOutCnt = outdelay + 1
      randCnt = outdelay + 1 + Random.nextInt(5)
    } else {
      poke(dut.en, 0)
      randCnt -= 1
    }
    if (inToOutCnt == 1) {
      expect(dut.out, expectedOutput(outCnt))
      outCnt += 1
    }
    if (inToOutCnt > 0) {
      inToOutCnt -= 1
    }

    step(1)
  }
}

object R22StageTester {
  def apply[T <: Data : Ring](proto: T, n: Int): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String]("-tiwv"),
      () => new R22Stage(proto, n)) { c =>
      new R22StageTester(c)
    }
  }
}

object BitRev {
  def apply(x: BigInt, nBits: Int): BigInt = {
    val trailingBits = x.toString(2)
    assert(trailingBits.length <= nBits)
    val allBits = "0" * (nBits - trailingBits.length) + trailingBits
    BigInt(allBits.reverse, 2)
  }
}

class R22SDFTester[T <: Data](dut: R22SDF[T], val freq: Int = 1) extends DspTester(dut) {
  val inputSeq = Seq.tabulate(dut.n) (i => Complex(
    math.cos(2 * math.Pi * freq * i.toDouble / dut.n),
    math.sin(2 * math.Pi * freq * i.toDouble / dut.n),
  ))
  val outputTonePos = BitRev(freq, log2Ceil(dut.n))
  println(s"Expected output tone pos = $outputTonePos")
  val expectedOutput = Seq.tabulate(dut.n)(x =>
    if (x == outputTonePos) {
      Complex(dut.n, 0)
    } else {
      Complex(0, 0)
    }
  )

  val input = inputSeq.iterator
  var output = Seq[Complex]()
  val expectedOutputIter = expectedOutput.iterator

  var totalCycles: Int = 0
  var inValidDelay: Int = 0
  var outReadyDelay: Int = 0

  poke(dut.in_last, 0)
  poke(dut.in.valid, 1)
  poke(dut.out.ready, 1)
  while (output.length < dut.n && totalCycles < 1000) {
    if (inValidDelay == 0 && input.hasNext) {
      if (input.hasNext && peek(dut.in.ready)) {
        poke(dut.in.valid, true)
        poke(dut.in.bits, input.next())
        inValidDelay = Random.nextInt(5)
      }
      // check if the previous poke was the last input
      poke(dut.in_last, !input.hasNext)
    } else {
      poke(dut.in.valid, false)
      inValidDelay -= 1
    }
    if (outReadyDelay == 0) {
      poke(dut.out.ready, true)
      if (peek(dut.out.valid)) {
        output :+= peek(dut.out.bits)
        if (expectedOutputIter.hasNext) {
          fixTolLSBs.withValue(2 + log2Ceil(dut.n)) {
            expect(dut.out.bits, expectedOutputIter.next())
          }
        } else {
          println(s"Getting some unexpected output: ${output.last}")
        }
        outReadyDelay = Random.nextInt(5)
      }
    } else {
      poke(dut.out.ready, false)
      outReadyDelay -= 1
    }

    step(1)
    totalCycles += 1
    // println(s"Output length = ${output.length} / ${dut.n}")
  }

  println(output.map(_.toString()).mkString(", "))
}

object R22SDFTester {
  def apply[T <: Data : Ring : ConvertableTo](n: Int, protoIn: T, protoOut: T, protoTwiddle: T, freq: Int = 1): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String]("-tiwv", "-tbn", "verilator"),
      () => new R22SDF(n = n, protoIn = protoIn, protoOut = protoOut, protoTwiddle = protoTwiddle)) { c =>
      new R22SDFTester(c, freq=freq)
    }
  }
}

class R22SDFSpec extends FlatSpec with Matchers {
  behavior of "BF2I"

  for (i <- 1 until 10) {
    it should s"work with delay = $i" in {
      BF2ITester(FixedPoint(32.W, 5.BP), i) should be (true)
    }
  }

  behavior of "BF2II"

  for (i <- 1 until 10) {
    it should s"work with delay = $i" in {
      BF2IITester(FixedPoint(32.W, 5.BP), i) should be (true)
    }
  }

  behavior of "R22Stage"

  for (i <- 1 until 5) {
    val n = math.pow(4.0, i).toInt
    for (delay <- 0 until 5) {
      it should s"work with n = $n and delay = $delay" in {
        DspContext.withNumAddPipes(delay) {
          R22StageTester(FixedPoint(32.W, 5.BP), n) should be (true)
        }
      }
    }
  }

  behavior of "R22SDF"

  val proto = FixedPoint(32.W, 24.BP)
  val protoTwiddle = FixedPoint(32.W, 30.BP)

  for (freq <- 0 until 4) {
    it should s"work for n = 4 with freq = $freq" in {
      DspContext.alter(DspContext.current.copy(
        numAddPipes = 1,
        numMulPipes = 3,
      )) {
        R22SDFTester(4, proto, proto, proto, freq=freq) should be(true)
      }
    }
  }

  for (freq <- 0 until 16) {
  // val freq = 13
     it should s"work for n = 16 with freq=$freq" in {
//  it should "work for n = 16" in {
      DspContext.alter(DspContext.current.copy(
        numAddPipes = 1,
        numMulPipes = 3,
      )) {
        R22SDFTester(16, proto, proto, protoTwiddle=protoTwiddle, freq=freq) should be(true)
      }
    }
  }

  for (freq <- 0 until 5) {
    it should s"work for n = 64 with freq = $freq" in {
      DspContext.alter(DspContext.current.copy(
        numAddPipes = 1,
        numMulPipes = 3,
      )) {
        R22SDFTester(64, proto, proto, protoTwiddle = protoTwiddle, freq = freq) should be(true)
      }
    }
  }

  for (freq <- 0 until 5) {
    it should s"work for n = 256 with freq = $freq" in {
      // shadowing is bad, mkay
      // this fft is big enough that the extra MSBs are nice
      val proto = FixedPoint(32.W, 20.BP)
      DspContext.alter(DspContext.current.copy(
        numAddPipes = 1,
        numMulPipes = 3,
      )) {
        R22SDFTester(256, proto, proto, protoTwiddle = protoTwiddle, freq = freq) should be(true)
      }
    }
  }
}
