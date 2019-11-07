package ofdm

import breeze.math.Complex
import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.DspTester
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

class SinglePointChannelEstimatorTester[T <: Data]
(dut: SinglePointChannelEstimator[T], in: Seq[(Complex, Complex)], expects: Seq[Complex], out: ArrayBuffer[Complex])
  extends DspTester(dut) {

  poke(dut.in.valid, 1)
  poke(dut.out.ready, 0) // start of waiting to fill output queue
  var draining = false
  val expectIterator = expects.iterator

  def checkOutput(): Unit = {
    if (peek(dut.out.valid)) {
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
      }
    }
  }
  for ((sample, pilot) <- in) {
    poke(dut.in.bits.pilot, pilot)
    poke(dut.in.bits.sample, sample)
    // if output buffer full, drain it

    while (!peek(dut.in.ready)) {
      poke(dut.out.ready, 1)
      draining = true
      checkOutput()
      step(1)
    }
    checkOutput()
    step(1)
  }
  poke(dut.in.valid, 0)
  var dead_cycles = 0
  while (dead_cycles < 10) {
    while (peek(dut.out.valid)) {
      dead_cycles = 0
      checkOutput()
      step(1)
    }
    dead_cycles += 1
    step(1)
  }
  step(100)
}

object SinglePointChannelEstimatorTester {
  def apply[T <: Data : RealBits](proto: T, in: Seq[(Complex, Complex)]): Seq[Complex] = {
    val protoComplex = DspComplex(proto)
    val p = RXParams(protoADC = protoComplex, protoAngle = proto, protoFFTIn = protoComplex, protoTwiddle = protoComplex,
      protoLLR = proto, maxNumPeaks = 64, timeStampWidth = 64, autocorrParams = AutocorrParams(protoComplex, 1, 1),
      ncoParams = NCOParams(10, 64, _ => proto, proto, proto), nFFT = 64)

    val expects = in.map({ case (sample, pilot) => pilot / sample })

    val out = ArrayBuffer[Complex]()

    chisel3.iotesters.Driver.execute(
      Array[String]("-tbn", "treadle", "-tiwv"),
      () => new SinglePointChannelEstimator(p)) {
      c => new SinglePointChannelEstimatorTester(c, in, expects, out)
    }

    out
  }
}

class FlatPilotEstimatorTester[T <: Data](dut: FlatPilotEstimator[T], out: ArrayBuffer[Seq[Complex]]) extends DspTester(dut) {
  def checkOutput(): Unit = {
    if (peek(dut.out.valid) && peek(dut.out.ready)) {
      val outvec = dut.out.bits.map(peek(_))
      out += outvec
      /*for () {
        val in = Complex()
        require()
      }*/
    }
  }
  poke(dut.out.ready, 1)
  poke(dut.in.valid, 1)

  val pilots = for (i <- 0 until dut.pilots.length) yield {
    Complex(
      math.cos(2.0 * math.Pi * i.toDouble / dut.pilots.length),
      math.sin(2.0 * math.Pi * i.toDouble / dut.pilots.length)
    )
  }
  for ((io, p) <- dut.pilots.zip(pilots)) {
    poke(io.real, p.real)
    poke(io.imag, p.imag)
  }

  for (i <- 0 until 5) {
    for (in <- dut.in.bits) {
      poke(in.real, math.pow(2.0, i) * math.cos(2 * math.Pi * i.toDouble / 5))
      poke(in.imag, math.pow(2.0, i) * math.sin(2 * math.Pi * i.toDouble / 5))
    }
    step(1)
    while (!peek(dut.in.ready)) {
      step(1)
      checkOutput()
    }
  }
  // checkOutput()
  // step(1)
  poke(dut.in.valid, 0)
  var dead_cycles = 0
  while (dead_cycles < 100) {
    while(peek(dut.out.valid)) {
      dead_cycles = 0
      checkOutput()
      step(1)
    }
    dead_cycles += 1
    step(1)
  }
}

object FlatPilotEstimatorTester {
  def apply[T <: Data : RealBits](proto: T): Seq[Seq[Complex]] = {
    val protoComplex = DspComplex(proto)
    val p = RXParams(protoADC = protoComplex, protoAngle = proto, protoFFTIn = protoComplex, protoTwiddle = protoComplex,
      protoLLR = proto, maxNumPeaks = 64, timeStampWidth = 64, autocorrParams = AutocorrParams(protoComplex, 1, 1),
      ncoParams = NCOParams(10, 64, _ => proto, proto, proto), pilotPos = Seq(4, 12, 20, 28, 36, 44, 52, 60), nFFT = 64)


    val out = ArrayBuffer[Seq[Complex]]()

    chisel3.iotesters.Driver.execute(
      Array[String]("-tbn", "treadle", "-tiwv"),
      () => new FlatPilotEstimator(p)) {
      c => new FlatPilotEstimatorTester[T](c, out)
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
    println("in=np.array([" + in.map(_._1).map(_.toString.replace("i", "j")).mkString(", ") + "])")
    println("out=np.array([" +
      SinglePointChannelEstimatorTester(proto, in).map(_.toString.replace("i", "j")).mkString(", ")
      + "])")
  }

  behavior of "FlatPilotEstimator"

  it should "estimate the channel coeffs with constant amplitude, rotating phase" in {
    val proto = FixedPoint(16.W, 10.BP)

    println("out=np.array([" +
      FlatPilotEstimatorTester(proto).map(
        "[" + _.map(_.toString.replace("i", "j")).mkString(", ") + "]").mkString(", ")
      + "])")

  }

}
