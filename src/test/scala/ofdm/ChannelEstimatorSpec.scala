package ofdm

import breeze.math.Complex
import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.DspTester
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

class ChannelEstimatorTester[T <: Data](dut: SinglePointChannelEstimator[T], in: Seq[(Complex, Complex)], expects: Seq[Complex], out: ArrayBuffer[Complex])
  extends DspTester(dut) {

  poke(dut.in.valid, 1)
  poke(dut.out.ready, 0) // start of waiting to fill output queue
  var draining = false
  val expectIterator = expects.iterator

  for ((sample, pilot) <- in) {
    // if output buffer full, drain it
    while (!peek(dut.in.ready)) {
      poke(dut.out.ready, 1)
      draining = true
      step(1)
    }
    poke(dut.in.bits.pilot, pilot)
    poke(dut.in.bits.sample, sample)
    if (draining && peek(dut.out.valid)) {
      out += peek(dut.out.bits)
      if (expectIterator.hasNext) {
        val exp = expectIterator.next()
        val eightBits = exp.abs / (1 << 6)
        dut.out.bits.real match {
          case f: FixedPoint => expectFixedPoint(f, exp.real, epsilon = eightBits, msg = "real")
          case _ =>
        }
        dut.out.bits.imag match {
          case f: FixedPoint => expectFixedPoint(f, exp.imag, epsilon = eightBits, msg = "imag")
          case _ =>
        }
        // expect(dut.out.bits, exp)
      }
    }
    step(1)
  }
  poke(dut.in.valid, 0)
  while (peek(dut.out.valid)) {
    out += peek(dut.out.bits)
    if (expectIterator.hasNext) {
      fixTolLSBs.withValue(10) {
        expect(dut.out.bits, expectIterator.next())
      }
    }
    step(1)
  }
  step(100)
}

object ChannelEstimatorTester {
  def apply[T <: Data : RealBits](proto: T, in: Seq[(Complex, Complex)]): Seq[Complex] = {
    val protoComplex = DspComplex(proto)
    val p = RXParams(
      protoADC = protoComplex,
      protoFFTIn = protoComplex,
      protoAngle = proto,
      protoTwiddle = protoComplex,
      nFFT = 64,
      maxNumPeaks = 64,
      timeStampWidth = 64,
      autocorrParams = AutocorrParams(protoComplex, 1, 1),
      ncoParams = NCOParams(10, 64, _ => proto, proto, proto)
    )

    val expects = in.map({ case (sample, pilot) => pilot / sample })

    val out = ArrayBuffer[Complex]()

    chisel3.iotesters.Driver.execute(Array[String]("-tbn", "verilator", "-tiwv"), () => new SinglePointChannelEstimator(p)) {
      c => new ChannelEstimatorTester(c, in, expects, out)
    }

    out
  }
}
class ChannelEstimatorSpec extends FlatSpec with Matchers {
  behavior of "SinglePointChannelEstimator"

  it should "work with FixedPoint" in {
    val proto = FixedPoint(16.W, 10.BP)

    val in = for (i <- 0 until 100) yield {
      (
        Complex((i + 1) / 10.0 * math.cos(2 * math.Pi * i / 100.0), (i + 1) / 10.0 * math.sin(2 * math.Pi * i / 100.0)),
        Complex(1.0, 0.0)
      )
    }
    println(ChannelEstimatorTester(proto, in))

  }

}
