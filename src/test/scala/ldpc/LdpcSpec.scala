package ldpc

import chisel3._
import chisel3.experimental.FixedPoint
import org.scalatest.{FlatSpec, Matchers}

class LdpcSpec extends FlatSpec with Matchers {

  behavior of "Encoder"

  it should "work for simple wikipedia examples" in {
    val generator = Seq(
      Seq(1, 0, 0, 1, 0, 1).map(_ != 0),
      Seq(0, 1, 0, 1, 1, 1).map(_ != 0),
      Seq(0, 0, 1, 1, 1, 0).map(_ != 0)
    )
    val inOuts = Seq(
      (Seq(0, 0, 0).map(_ != 0), Seq(0, 0, 0, 0, 0, 0).map(_ != 0)),
      (Seq(0, 0, 1).map(_ != 0), Seq(0, 0, 1, 1, 1, 0).map(_ != 0)),
      (Seq(0, 1, 0).map(_ != 0), Seq(0, 1, 0, 1, 1, 1).map(_ != 0)),
      (Seq(0, 1, 1).map(_ != 0), Seq(0, 1, 1, 0, 0, 1).map(_ != 0)),
      (Seq(1, 0, 0).map(_ != 0), Seq(1, 0, 0, 1, 0, 1).map(_ != 0)),
      (Seq(1, 0, 1).map(_ != 0), Seq(1, 0, 1, 0, 1, 1).map(_ != 0)),
      (Seq(1, 1, 0).map(_ != 0), Seq(1, 1, 0, 0, 1, 0).map(_ != 0)),
      (Seq(1, 1, 1).map(_ != 0), Seq(1, 1, 1, 1, 0, 0).map(_ != 0)),
    )
    EncoderTester(generator, inOuts) should be (true)
  }

  it should "work for CCSDS (128, 64) example" in {
    val generator = Generator.fromHexString(16, 128,
      "0E69166BEF4C0BC2" +
      "7766137EBB248418" +
      "C480FEB9CD53A713" +
      "4EAA22FA465EEA11"
    )
    // println(s"Generator is ${generator.size} x ${generator.head.size}")
    // println(generator.head.drop(64).mkString(", "))
    val inOuts = Seq(
      (Seq.fill(64)(false), Seq.fill(128)(false)),
      (Seq(true) ++ Seq.fill(63)(false),
        Seq(
          1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
          0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
          0,0,0,0, 1,1,1,0, 0,1,1,0, 1,0,0,1, 0,0,0,1, 0,1,1,0, 0,1,1,0, 1,0,1,1,
          1,1,1,0, 1,1,1,1, 0,1,0,0, 1,1,0,0, 0,0,0,0, 1,0,1,1, 1,1,0,0, 0,0,1,0).map(_ != 0)),
      (Seq(false, true) ++ Seq.fill(62)(false),
        Seq(
          0,1,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
          0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
          1,0,0,0,0, 1,1,1,0, 0,1,1,0, 1,0,0, 1,0,0,0,1, 0,1,1,0, 0,1,1,0, 1,0,1,
          0,1,1,1,0, 1,1,1,1, 0,1,0,0, 1,1,0, 0,0,0,0,0, 1,0,1,1, 1,1,0,0, 0,0,1).map(_ != 0)),
      // (Seq.fill(64)(true),  )
    )
    EncoderTester(generator, inOuts) should be (true)
  }

  behavior of "Bitonic Sorter"

  for (i <- 1 until 6) {
    val n = 1 << i
    it should s"work for n = $n" in {
      BitonicSorterTester(n) should be (true)
    }
  }

  behavior of "Shifter"

  for (i <- 2 until 32) {
    it should s"work for n = $i" in {
      ShifterTester(i) should be (true)
    }
  }

  behavior of "Params"

  it should "compute a good schedule" in {

    for (e <- CCSDS.params64x128.schedule) {
      println(e.entries.mkString(", "))
    }
  }

  behavior of "BPDecoder"

  it should "decode small CCSDS (128, 64) code" in {
    BPDecoderTester(FixedPoint(6.W, 2.BP), CCSDS.params64x128, nTrials = 500, ebn0 = 3.0) should be (true)
  }

  it should "decode the medium CCSDS (256, 128) code" in {
    BPDecoderTester(FixedPoint(6.W, 2.BP), CCSDS.params128x256, nTrials = 500, ebn0 = 3.0) should be (true)
  }

  it should "decode larger CCSDS (512, 256) code" in {
    BPDecoderTester(FixedPoint(6.W, 2.BP), CCSDS.params256x512, nTrials = 500, ebn0 = 3.0) should be (true)
  }
}
