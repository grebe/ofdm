package ofdm

import breeze.math.Complex
import chisel3._
import chisel3.experimental._
import dsptools.DspTester
import dsptools.numbers._
import ieee80211.{ADITrace, IEEE80211}
import org.scalatest.{FlatSpec, Matchers}

class SyncSpec extends FlatSpec with Matchers {

  behavior of "Sync Block"


  def runTest[T <: Data : Real : BinaryRepresentation](signal: Seq[Complex], params: SyncParams[T], thresh: Double = 0.5): Seq[(Complex, Int)] = {
    var output = Seq[(Complex, Int)]()

    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), () => new Sync(params)) {
      c =>
        new DspTester(c) {
          updatableSubVerbose.withValue(true) {
            updatableDspVerbose.withValue(true) {
              step(5)
              poke(c.io.autocorrConfig.depthApart, 64)
              poke(c.io.autocorrConfig.depthOverlap, 64)
              poke(c.io.peakDetectConfig.peakDistance, 160)
            }
          }

          poke(c.io.in.valid, 1)
          for ((in, time) <- signal.zipWithIndex) {
            poke(c.io.in.bits.stream, in)
            poke(c.io.in.bits.time, time)
            step(1)
            if (peek(c.io.out.valid)) {
              output = output :+ (peek(c.io.out.bits.stream), peek(c.io.out.bits.time))
            }
          }
          poke(c.io.in.bits.stream, Complex(0.0, 0.0))
          for (_ <- 0 until 100) {
            step(1)
            if (peek(c.io.out.valid)) {
              output = output :+ (peek(c.io.out.bits.stream), peek(c.io.out.bits.time))
            }
          }
        }
    }
    output
  }

  val protoIn  = FixedPoint(16.W, 14.BP)
  val protoOut = FixedPoint(16.W, 14.BP)
  val stfParams = SyncParams(
    protoIn = DspComplex(protoIn),
    protoOut = DspComplex(protoOut),
    maxNumPeaks = 256,
    timeStampWidth = 64,
    autocorrParams = AutocorrParams(
      protoIn = DspComplex(protoIn),
      maxApart = 256,
      maxOverlap = 256
    )
  )

  it should "correct no CFO with STF" ignore {
    val testSignal = IEEE80211.stf ++ Seq.fill(500) { Complex(0.125, 0) }
    val cfoSignal = IEEE80211.addCFO(testSignal, 0.0e3)

    val output = runTest(cfoSignal, stfParams)


    println(s"Input was:")
    println(cfoSignal.toString)
    println(s"Output was:")
    println(output.toString)
}

  it should "correct CFO of 50 kHz with STF" in {
    val testSignal = IEEE80211.stf ++ Seq.fill(500) { Complex(0.125, 0) }
    val cfoSignal = IEEE80211.addCFO(testSignal, -50.0e3)

    val output = runTest(cfoSignal, stfParams)


    println(s"Input was:")
    println(cfoSignal.toString)
    println(s"Output was:")
    println(output.toString)
 }

  it should "correct CFO for Rimas's test signal" ignore {
    val testSignal = ADITrace.textResource("/waveforms/wifi_bpsk.txt")
    val cfoSignal = IEEE80211.addCFO(testSignal, -50.0e3)

    val output = runTest(cfoSignal, stfParams, thresh = .025)


    println(s"Input was:")
    println(cfoSignal.toString)
    println(s"Output was:")
    println(output.toString)
}

  it should "correct CFO for Rimas's test signal (digital loopback)" ignore {
    val testSignal = ADITrace.binaryResource("/waveforms/wifi-bpsk-loopback-digital.dat.xz")
    val cfoSignal = testSignal //IEEE80211.addCFO(testSignal, -50.0e3)

    val output = runTest(cfoSignal, stfParams, thresh = .025)


    println(s"Input was:")
    println(cfoSignal.toString)
    println(s"Output was:")
    println(output.toString)

  }

  it should "correct CFO for Rimas's test signal (cable loopback)" ignore {
    val testSignal = ADITrace.binaryResource("/waveforms/wifi-bpsk-loopback-cable.dat.xz").take(100000)
    val cfoSignal = testSignal //IEEE80211.addCFO(testSignal, -50.0e3)

    val output = runTest(cfoSignal, stfParams, thresh = 0.2)


    println(s"Input was:")
    println(cfoSignal.toString)
    println(s"Output was:")
    println(output.toString)

  }

  it should "correct CFO for Rimas's test signal (two board)" in {
    val testSignal = ADITrace.binaryResource("/waveforms/wifi-bpsk-2boards.dat.xz").take(100000)
    val cfoSignal = testSignal //IEEE80211.addCFO(testSignal, -50.0e3)

    val output = runTest(cfoSignal, stfParams, thresh = 0.2)


    println(s"Input was:")
    println(cfoSignal.toString)
    println(s"Output was:")
    println(output.toString)
  }

}
