package ldpc

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.util.log2Ceil

class ShifterTester(dut: Shifter[UInt]) extends PeekPokeTester(dut) {
  dut.in.zipWithIndex.foreach { case (in, idx) => poke(in, idx) }

  for (shift <- 0 until dut.n) {
    poke(dut.shift, shift)
    dut.out.zipWithIndex.foreach { case (o, idx) =>
        expect(o, (idx + shift) % dut.n)
    }

    step(1)
  }
}

object ShifterTester {
  def apply(n: Int): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String](), () => new Shifter(UInt(log2Ceil(n).W), n)) { c =>
      new ShifterTester(c)
    }
  }
}
