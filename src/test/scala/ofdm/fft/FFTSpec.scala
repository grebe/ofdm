// Originally written for Fall 2018 EE290C modem class project
// Modified by Paul Rigge
package ofdm
package fft

import breeze.linalg.{DenseVector, randomDouble}
import breeze.math.Complex
import breeze.signal.{fourierTr, iFourierTr}
import chisel3.experimental.FixedPoint
import org.scalatest.{FlatSpec, Matchers}

class FFTSpec extends FlatSpec with Matchers {

  def test_setup(base_params: FFTParams[FixedPoint], numPoints: Int, fftType: FFTType): (FFTParams[FixedPoint], Vector[Complex], Vector[Complex], Vector[Complex]) = {
    val inp      = DenseVector.fill(numPoints) { Complex(randomDouble() * 2 - 1, randomDouble() * 2 - 1) }
    val out_fft  = fourierTr(inp).toScalaVector
    val out_ifft = iFourierTr(inp).toScalaVector
    val new_params = base_params.copy(numPoints = numPoints, fftType = fftType)
    (new_params, inp.toScalaVector, out_fft, out_ifft)
  }

  behavior of "FixedFFT"

  val base_params = FFTParams.fixed(
    dataWidth = 10,
    twiddleWidth = 10,
    numPoints = 2,
    binPoint = 2,
    sdfRadix = 2
  )

  for (i <- Seq(2, 16, 64)) {
    it should f"compute $i-point Direct FFT/IFFT (powers of 2)" in {
      val (params, inp, out_fft, out_ifft) = test_setup(base_params, i, DirectFFTType)
      FixedFFTTester(params,  inp, out_fft ) should be (true)
      FixedIFFTTester(params, inp, out_ifft) should be (true)
    }
    it should f"compute $i-point radix-2 SDF FFT/IFFT" in {
      val (params, inp, out_fft, out_ifft) = test_setup(base_params, i, SDFFFTType)
      FixedFFTTester(params,  inp, out_fft ) should be (true)
      FixedIFFTTester(params, inp, out_ifft) should be (true)
    }
  }

  for (i <- Seq(4, 16, 64)) {
    it should f"compute $i-point radix-4 SDF FFT/IFFT" in {
      val (radix_2_params, inp, out_fft, out_ifft) = test_setup(base_params, i, SDFFFTType)
      val params = radix_2_params.copy(sdfRadix = 4)
      FixedFFTTester(params,  inp, out_fft ) should be (true)
      FixedIFFTTester(params, inp, out_ifft) should be (true)
    }
  }

  for (i <- Seq(3, 9, 81)) {
    it should f"compute $i-point Direct FFT/IFFT (powers of 3)" in {
      val (params, inp, out_fft, out_ifft) = test_setup(base_params, i, DirectFFTType)
      FixedFFTTester(params,  inp, out_fft ) should be (true)
      FixedIFFTTester(params, inp, out_ifft) should be (true)
    }
  }

  for (i <- Seq(6, 15, 36)) {
    it should f"compute $i-point Direct FFT/IFFT" in {
      val (params, inp, out_fft, out_ifft) = test_setup(base_params, i, DirectFFTType)
      FixedFFTTester(params,  inp, out_fft ) should be (true)
      FixedIFFTTester(params, inp, out_ifft) should be (true)
    }
  }

  behavior of "FFTUtil"
  it should "check factorize" in {
    val inputs = List(12, 30, 25)
    val output_factors = List(List(2, 3), List(2, 3, 5), List(5))
    val output_powers  = List(List(2, 1), List(1, 1, 1), List(2))
    inputs.zip(output_factors.zip(output_powers)).foreach {
      case (inp, out) => {
        assert(FFTUtil.factorize(inp) == out)
      }
    }
  }
  it should "check gcd extended" in {
    val as   = List(30, 35)
    val bs   = List(20, 15)
    val gcds = List(10, 5)
    val xs   = List(1 , 1)
    val ys   = List(-1, -2)
    val inputs = as.zip(bs)
    val outputs = (gcds, xs, ys).zipped.toList
    inputs.zip(outputs).foreach {
      case ((a, b), (gcd, x, y)) => {
        assert(FFTUtil.gcd_extended(a, b) == (gcd, x, y))
      }
    }
  }
  it should "check mult inv" in {
    val inputs = List(List(2, 13), List(4, 13))
    val outputs = List(7, 10)
    inputs.zip(outputs).foreach {
      case (inp, out) => {
        assert(FFTUtil.mult_inv(inp(0), inp(1)) == out)
      }
    }
  }
  it should "check primitive root" in {
    val inputs  = List(13, 17)
    val outputs = List(2 , 3)
    inputs.zip(outputs).foreach {
      case (inp, out) => {
        assert(FFTUtil.primitive_root(inp) == out)
      }
    }
  }
  it should "check is prime" in {
    val inputs  = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val outputs = List(false, true, true, false, true, false, true, false, false, false)
    inputs.zip(outputs).foreach {
      case (inp, out) => {
        assert(FFTUtil.is_prime(inp) == out)
      }
    }
  }
  it should "check is power" in {
    val inputs  = List(2, 3, 4, 5, 6, 7, 8, 9, 10)
    val outputs = List(true, true, true, true, false, true, true, true, false)
    inputs.zip(outputs).foreach {
      case (inp, out) => {
        assert(FFTUtil.is_power(inp) == out)
      }
    }
  }
}
