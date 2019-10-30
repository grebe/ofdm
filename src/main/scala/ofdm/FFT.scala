package ofdm

import chisel3._
import chisel3.util._
import breeze.math.Complex
import chisel3.experimental.{BaseModule, MultiIOModule}
import chisel3.iotesters.PeekPokeTester
import dsptools.DspTester
import dsptools.numbers._

class FixedButterflyIO[T <: Data : Ring](genIn: T, genOut: T, n: Int) extends Bundle {
  val in       = Input (Vec(n, DspComplex(genIn, genIn)))
  val out      = Output(Vec(n, DspComplex(genOut, genOut)))
}

class FixedButterfly[T <: Data : Ring : ConvertableTo](genIn: T, genOut: T, genTwiddle: T, twiddles: Map[Int, Seq[Complex]], n: Int) extends Module {
  val io = IO(new FixedButterflyIO(genIn, genOut, n))

  for (outIdx <- 0 until n) {
    require(twiddles.contains(outIdx), s"Twiddles don't contain $outIdx")
    val out = io.out(outIdx)
    val t = twiddles(outIdx)
    require(t.length == n, s"Twiddles should be length $n, got ${t.length}")

    val terms = io.in.zip(t) map { case (input, twiddle) =>
      twiddle match {
        case Complex(0, 0) => None
        case Complex(1, 0) => Some(input)
        case Complex(-1, 0) => Some(-input)
        case Complex(0, 1) => Some(input.mulj())
        case Complex(0, -1) => Some(-input.mulj())
        case Complex(r, i) =>
          val real = ConvertableTo[T].fromDouble(r, genTwiddle)
          val imag = ConvertableTo[T].fromDouble(i, genTwiddle)
          Some(input * DspComplex(real, imag))
      }
    } collect { case Some(x) => x }

    require(terms.nonEmpty, s"Output $outIdx has no twiddles!")

    out := TreeReduce(terms, (x: DspComplex[T], y: DspComplex[T]) => x + y)
  }
}

class Butterfly2[T <: Data : Ring](genIn: T, genOut: Option[T] = None) extends MultiIOModule {
  val in0  = IO(Input (genIn.cloneType))
  val in1  = IO(Input (genIn.cloneType))

  val sum  = in0 + in1
  val diff = in0 - in1

  val out0 = IO(Output(genOut.getOrElse(sum).cloneType))
  val out1 = IO(Output(genOut.getOrElse(diff).cloneType))

  out0 := sum
  out1 := diff
}

class StreamingFFTIO[T <: Data : Ring](genIn: T, genOut: T) extends Bundle {
  val in   = Input (Valid(DspComplex(genIn, genIn)))
  val last = Input(Bool())
  val out  = Output(Valid(DspComplex(genOut, genOut)))
}

trait HasStreamingFFTIO[T <: Data] {
  def io: StreamingFFTIO[T]
}

object SimulateStreamingFFT {
  def apply[T <: Data, M <: Module with HasStreamingFFTIO[T]]
  (
    dut: () => M,
    in: Seq[Complex],
    expects: Option[Seq[Complex]] = None,
    tolerance: Int = 14
  ): Seq[Complex] = {
    var out: Seq[Complex] = Seq()
    var expectsMutable = expects.getOrElse(Seq())
    dsptools.Driver.execute( dut, Array[String]("-tbn", "verilator") ) { c => new DspTester(c) {
      def checkOutput(): Unit = {
        fixTolLSBs.withValue(tolerance) {
          if (peek(c.io.out.valid)) {
            out = out :+ peek(c.io.out.bits)
            if (expectsMutable.nonEmpty) {
              expect(c.io.out.bits, expectsMutable.head)
              expectsMutable = expectsMutable.tail
            }
          }
        }
      }
      poke(c.io.in.valid, 1)
      for (i <- in) {
        poke(c.io.in.bits, i)
        step(1)
        checkOutput()
      }
      for (_ <- in.indices) {
        step(1)
        checkOutput()
      }
    }}
    out
  }
}

class R2SDF[T <: Data : Ring : ConvertableTo](val n: Int, val genIn: T, val genOut: T, val genTwiddle: Option[T] = None)
  extends Module with HasStreamingFFTIO[T] {

  require(isPow2(n), s"Radix 2 FFT must be power of 2")

  val io = IO(new StreamingFFTIO(genIn, genOut))

  val nStages = log2Ceil(n)
  val delays = (1 to nStages).map { n >> _ }

  val en = io.in.valid

  val genTwiddleResolved = genTwiddle.getOrElse(genIn)

  io.out.bits := delays.foldLeft(io.in.bits) { case (in, delay) =>
    val bf = Module(new Butterfly2(in))

    val shifted = if (delay <= 1 || true) {
      ShiftRegister(bf.out0, delay, en = en)
    } else {
      val shr = Module(new ShiftRegisterMem(chiselTypeOf(in), delay))
      shr.io.in.valid := en
      shr.io.in.bits  := bf.out0
      shr.io.depth.valid := reset
      shr.io.depth.bits := delay.U
      shr.io.out.bits
    }

    bf.in0 := shifted
    if (delay == 1) {
      bf.in1 := in.mulj()
    } else {
      bf.in1 := in
    }

    val twiddlesComplex = Seq.tabulate(delay) { i => RootsOfUnity(i * n / delay, n) }
    val twiddlesDspComplex = twiddlesComplex.map( c => DspComplex.wire(
      ConvertableTo[T].fromDoubleWithFixedWidth(c.real, genTwiddleResolved),
      ConvertableTo[T].fromDoubleWithFixedWidth(c.imag, genTwiddleResolved)
    ))

    val twiddles = VecInit(twiddlesDspComplex)
    val (twiddleCount, _) = Counter(en, delay * 2)

    val product =  bf.out1 * twiddles(twiddleCount)
    val out = Wire(chiselTypeOf(product))

    when (twiddleCount < delay.U) {
      out := DspComplex.wire(Ring[T].zero, Ring[T].zero)
    } .otherwise {
      out := product
    }

    out
  }

  io.out.valid := ShiftRegister(io.in.valid, n, false.B, true.B)
}
