package ldpc

import breeze.numerics.pow
import chisel3._
import dsptools.DspTester
import dsptools.numbers._

import scala.util.Random

class BPDecoderTester[T <: Data](dut: BPDecoder[T], nTrials: Int = 100, ebn0: Seq[Double] = Seq(1.0)) extends DspTester(dut) {
  updatableDspVerbose.withValue(false) {
    var errors = 0
    var perrors = 0
    var nchecked = 0

    def checkOutput(input: Seq[Boolean], hasChecked: Boolean): Boolean = {
      if (hasChecked) {
        return hasChecked
      }

      poke(dut.out.ready, true)
      if (peek(dut.out.valid)) {
        var sawError = false
        for ((o, i) <- dut.out.bits.zip(input)) {
          // expect(o, i)
          if (peek(o) != i) {
            errors += 1
            sawError = true
          }
        }
        if (sawError) {
          perrors += 1
        }
        nchecked += 1
        return true
      }

      return false
    }

    for (e <- ebn0) {
      var checked = false
      errors  = 0
      perrors = 0
      nchecked = 0

      for (trial <- 0 until nTrials) {
        checked = false
        val input: Seq[Boolean] = for (_ <- 0 until dut.params.k) yield scala.util.Random.nextBoolean()
        val codeword = SWEncoder(input, dut.params)
        // println(codeword.map(_.toInt).mkString(", "))
        val withNoise = codeword.map(cw =>
          (2 * cw.toInt - 1) + math.sqrt(1.0 / pow(10.0, e / 10.0)) * Random.nextGaussian())
        // val eee = withNoise.map(_ >= 0.0).zip(codeword).map({ case (n, c) => (n == c).toInt }).sum
        // println(s"Errors = $eee")

        for ((i, ncw) <- dut.in.bits.zip(withNoise)) {
          poke(i, ncw)
        }

        poke(dut.in.valid, 1)

        checked = checkOutput(input, checked)
        step(1)
        while (!peek(dut.in.ready)) {
          checked = checkOutput(input, checked)
          step(1)
        }
        poke(dut.in.valid, 0)
        while (!checked) {
          checked = checkOutput(input, checked)
          step(1)
        }
      }
      println(s"ebn0 = $e, errors = $errors, perrors = $perrors, nchecked = $nchecked, total = ${nTrials * dut.params.k}, BER = ${errors.toDouble / (nTrials * dut.params.k)}, PER = ${perrors.toDouble / nTrials}")
    }

  }
}

object BPDecoderTester {
  def apply[T <: Data : Real](protoLLR: T, params: LdpcParams, nTrials: Int = 100, ebn0: Seq[Double] = Seq(1.0)): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String]("-tbn", "verilator"), () =>
      new BPDecoder(protoLLR, params)) { c =>
      new BPDecoderTester(c, nTrials = nTrials, ebn0 = ebn0)
    }
  }
}