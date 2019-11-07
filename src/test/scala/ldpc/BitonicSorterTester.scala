package ldpc

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.util.log2Ceil

class BitonicSorterTester(dut: BitonicSorter[UInt], maxTests: Int = 500) extends PeekPokeTester(dut) {
  val actualMaxTests = (BigInt(1) << dut.n).min(maxTests).toInt

  for (_ <- 0 until actualMaxTests) {
    val perm = scala.util.Random.shuffle((0 until dut.n).toList)
    perm.zip(dut.in).foreach { case (p, i) => poke(i, p) }
    dut.out.zipWithIndex.foreach { case (o, e) => expect(o, e) }
    step(1)
  }
}

object BitonicSorterTester {
  def apply(n: Int, maxTests: Int = 500): Boolean = {
    chisel3.iotesters.Driver.execute(Array[String](), () => new BitonicSorter(UInt(log2Ceil(n).W), n = n)) { c =>
      new BitonicSorterTester(c, maxTests = maxTests)
    }
  }
}