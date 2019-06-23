package ofdm

import breeze.math.Complex
import chisel3._
import chisel3.experimental._
import dsptools.DspTester
import dsptools.numbers._
import ieee80211.{ADITrace, IEEE80211}
import org.scalatest.{FlatSpec, Matchers}

class SyncSpec extends FlatSpec with Matchers {

  behavior of "Time-domain RX"


  def runTest[T <: Data : Real : BinaryRepresentation](signal: Seq[Complex], params: RXParams[T], thresh: Double = 0.5): Seq[(Complex, Int)] = {
    var output = Seq[(Complex, Int)]()

    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"), () => new TimeDomainRX(params)) {
      c => new DspTester(c) {
          updatableSubVerbose.withValue(true) {
            updatableDspVerbose.withValue(true) {
              step(5)
              poke(c.autocorrConfig.depthApart, 64)
              poke(c.autocorrConfig.depthOverlap, 64)
              poke(c.peakDetectConfig.peakDistance, 160)
            }
          }

          poke(c.in.valid, 1)
          for (in <- signal) {
            poke(c.in.bits, in)
            step(1)
            if (peek(c.out.valid)) {
              output = output :+ (peek(c.out.bits.stream), peek(c.out.bits.time))
            }
          }
          poke(c.in.bits, Complex(0.0, 0.0))
          for (_ <- 0 until 100) {
            step(1)
            if (peek(c.out.valid)) {
              output = output :+ (peek(c.out.bits.stream), peek(c.out.bits.time))
            }
          }
        }
    }
    output
  }

  val protoIn  = FixedPoint(16.W, 14.BP)
  val protoOut = FixedPoint(16.W, 14.BP)
  val protoAngle = FixedPoint(16.W, 13.BP)
  val protoFreq = FixedPoint(16.W, 8.BP)

  val stfParams = RXParams(
    protoIn = DspComplex(protoIn),
    protoOut = DspComplex(protoOut),
    protoAngle = protoAngle,
    maxNumPeaks = 256,
    timeStampWidth = 64,
    autocorrParams = AutocorrParams(
      protoIn = DspComplex(protoIn),
      maxApart = 256,
      maxOverlap = 256
    ),
    ncoParams = NCOParams(
      phaseWidth = 16,
      tableSize = 64,
      phaseConv = u => u.asTypeOf(protoAngle),
      protoFreq = protoFreq,
      protoOut = protoOut,
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
