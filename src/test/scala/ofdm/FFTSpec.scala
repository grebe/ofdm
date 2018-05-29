package ofdm

import breeze.math.Complex
import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import org.scalatest._

class FFTSpec extends FlatSpec with Matchers {
  behavior of "R2SDF"

  it should "work with floating point" in {
    val N = 4
    val f = 0
    val cosine = Seq.tabulate(N*4) { i => Complex(math.cos(i * 2 * math.Pi * f / N), 0.0) }
    println(
      SimulateStreamingFFT[DspReal, R2SDF[DspReal]](
        () => new R2SDF(N, DspReal(), DspReal()),
        cosine
      )
    )
  }

  it should "work with fixed point" in {
    val genIn = FixedPoint(16.W, 8.BP)
    val genOut = genIn
    val N = 4
    val f = 0
    val cosine = Seq.tabulate(N*4) { i => Complex(math.cos(i * 2 * math.Pi * f / N), 0.0) }
    println(
      SimulateStreamingFFT[FixedPoint, R2SDF[FixedPoint]](
        () => new R2SDF(N, genIn, genOut),
        cosine
      ).map(_.abs)
    )

  }

}
