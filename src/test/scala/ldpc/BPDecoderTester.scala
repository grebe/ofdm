package ldpc

import chisel3._
import dsptools.DspTester
import dsptools.numbers._

class BPDecoderTester[T <: Data](dut: BPDecoder[T], nTrials: Int = 100, ebn0: Double = 1.0) extends DspTester(dut) {
  var errors = 0
  var checked = false

  def checkOutput(input: Seq[Boolean], hasChecked: Boolean): Boolean = {
    if (hasChecked) {
      poke(dut.out.ready, false)
      return hasChecked
    }

    poke(dut.out.ready, true)
    if (peek(dut.out.valid)) {
      for ((o, i) <- dut.out.bits.zip(input)) {
        expect(o, i)
      }
      return true
    }

    return false
  }

  for (_ <- 0 until nTrials) {
    val input = for (_ <- 0 until dut.params.k) yield scala.util.Random.nextBoolean()
    val codeword = SWEncoder(input, dut.params)

    for ((i, cw) <- dut.in.bits.zip(codeword)) {
      poke(i, cw.toDouble)
    }

    poke(dut.in.valid, 1)

    step(1)
    while (!peek(dut.in.ready)) {
      checked = checkOutput(input, checked)
      step(1)
    }
  }
}

object BPDecoderTester {
  def apply[T <: Data : Real](protoLLR: T, params: LdpcParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String](), () => new BPDecoder(protoLLR, params)) { c =>
      new BPDecoderTester(c)
    }
  }
}