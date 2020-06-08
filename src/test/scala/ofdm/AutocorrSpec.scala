package ofdm

import chisel3._
import chisel3.iotesters.TesterOptionsManager
import dsptools.numbers.implicits._
import org.scalatest.{FlatSpec, Matchers}

class AutocorrSpec extends FlatSpec with Matchers {
  val manager = new TesterOptionsManager {
    interpreterOptions = interpreterOptions.copy(setVerbose = false, writeVCD = true)
    testerOptions = testerOptions.copy(backendName = "treadle", generateVcdOutput = "on")
  }

  def overlapDut(width:Int, depth:Int, delay: Int): () => OverlapSum[UInt] =
    () => new OverlapSum(UInt(width.W), depth, delay)

  def shrDut(width: Int, depth: Int): () => ShiftRegisterMem[UInt] =
    () => new ShiftRegisterMem(UInt(width.W), depth)

  behavior of "OverlapSum"

  for (d <- 1 until 10) {
    it should s"work with depth $d" in {
      iotesters.Driver.execute(overlapDut(8, 10, 0), optionsManager = manager) { c =>
        new OverlapSumTester(c, d) } should be (true)
    }
  }

  // it should "work with depth 1" in {
  //   iotesters.Driver.execute(overlapDut(8, 4, 0), optionsManager = manager) { c =>
  //     new OverlapSumTester(c, 1) } should be (true)
  // }

  // it should "work with full depth" in {
  //   iotesters.Driver.execute(overlapDut(8, 5, 0), optionsManager = manager) { c =>
  //     new OverlapSumTester(c, 4) } should be (true)
  // }

  // it should "work with pipeline delays" in {
  //   iotesters.Driver.execute(overlapDut(8, 4, 5), optionsManager = manager) { c =>
  //     new OverlapSumTester(c, 4) } should be (true)
  // }


  behavior of "ShiftRegister"

  it should "work with depth 1" in {
    iotesters.Driver.execute(shrDut(8, 4), optionsManager = manager) { c =>
      new ShiftRegisterTester(c, 1)
    } should be (true)
  }

  it should "work with full depth" in {
    iotesters.Driver.execute(shrDut(8, 8), optionsManager = manager) { c =>
      new ShiftRegisterTester(c, 7)
    } should be (true)
  }

}
