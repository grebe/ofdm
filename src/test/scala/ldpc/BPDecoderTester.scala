package ldpc

import breeze.numerics.pow
import chisel3._
import dsptools.DspTester
import dsptools.numbers._

import scala.util.Random

class BPDecoderTester[T <: Data](dut: BPDecoder[T], nTrials: Int = 100, ebn0: Double = 1.0) extends DspTester(dut) {
  updatableDspVerbose.withValue(false) {
    var errors  = 0
    var checked = false

    def checkOutput(input: Seq[Boolean], hasChecked: Boolean): Boolean = {
      if (hasChecked) {
        return hasChecked
      }

      poke(dut.out.ready, true)
      if (peek(dut.out.valid)) {
        for ((o, i) <- dut.out.bits.zip(input)) {
          // expect(o, i)
          if (peek(o) != i) {
            errors += 1
          }
        }
        return true
      }

      return false
    }

    for (_ <- 0 until nTrials) {
      val input    = for (_ <- 0 until dut.params.k) yield scala.util.Random.nextBoolean()
      val codeword = SWEncoder(input, dut.params)

      for ((i, cw) <- dut.in.bits.zip(codeword)) {
        poke(i, (2 * cw.toDouble - 1) * pow(10.0, ebn0 / 10.0) + Random.nextGaussian())
      }

      poke(dut.in.valid, 1)

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

    println(s"errors = $errors, total = ${nTrials * dut.params.k}, BER = ${errors.toDouble / (nTrials * dut.params.k)}")
  }
}

object BPDecoderTester {
  def apply[T <: Data : Real](protoLLR: T, params: LdpcParams, nTrials: Int = 100, ebn0: Double = 1.0): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String](), () =>
      new BPDecoder(protoLLR, params)) { c =>
      new BPDecoderTester(c, nTrials = nTrials, ebn0 = ebn0)
    }
  }
}