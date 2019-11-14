package ofdm

import breeze.math.Complex
import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.{DspContext, DspTester}
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}

class R22SDFTester[T <: Data](dut: R22SDF[T]) extends DspTester(dut) {
  val freq = 7

  val inputSeq = Seq.tabulate(dut.n) (i => Complex(
    math.cos(2 * math.Pi * freq * i.toDouble / dut.n),
    math.sin(2 * math.Pi * freq * i.toDouble / dut.n),
  ))
  val input = inputSeq.iterator
  var output = Seq[Complex]()

  poke(dut.in.valid, 1)
  poke(dut.out.ready, 1)
  while (output.length < dut.n) {
    if (input.hasNext && peek(dut.in.ready)) {
      poke(dut.in.bits, input.next())
    }
    if (peek(dut.out.valid)) {
      output = output :+ peek(dut.out.bits)
    }
    step(1)
  }

  println(output.map(_.toString()).mkString(", "))
}

object R22SDFTester {
  def apply[T <: Data : Ring : ConvertableTo](n: Int, protoIn: T, protoOut: T, protoTwiddle: T): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String]("-tiwv"),
      () => new R22SDF(n = n, protoIn = protoIn, protoOut = protoOut, protoTwiddle = protoTwiddle)) { c =>
      new R22SDFTester(c)
    }
  }
}

class R22SDFSpec extends FlatSpec with Matchers {
  behavior of "R22SDF"

  val proto = FixedPoint(16.W, 8.BP)

  it should "work for n = 16" in {
    DspContext.alter(DspContext.current.copy(
      numAddPipes = 1,
      numMulPipes = 3,
    )) {
      R22SDFTester(16, proto, proto, proto) should be(true)
    }
  }
}
