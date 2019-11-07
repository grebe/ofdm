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

  def seqToString(c: Seq[Complex]): String =
    "[" +
    c.map(i => s"${i.real}+${i.imag}j").mkString(", ") +
    "]"

  def runTest[T <: Data : Real : BinaryRepresentation](signal: Seq[Complex], params: RXParams[T], thresh: Double = 0.5): Seq[(Complex, Int)] = {
    var output = Seq[(Complex, Int)]()

    dsptools.Driver.execute(() => new TimeDomainRX(params), Array("-tbn", "verilator", "-dtinv")) {
      c => new DspTester(c) {
        updatableSubVerbose.withValue(true) {
          updatableDspVerbose.withValue(true) {
            poke(c.in.valid, 0)
            poke(c.out.ready, 0)

            poke(c.tlast, 0)
            poke(c.autocorrConfig.depthApart, 64)
            poke(c.autocorrConfig.depthOverlap, 64)
            poke(c.peakDetectConfig.numPeaks, 70)
            poke(c.peakDetectConfig.peakDistance, 160)
            poke(c.mutatorCommandIn.bits.length, 200)
            poke(c.peakThreshold, 0.5)
            poke(c.peakOffset, 0.0005)

            poke(c.mutatorCommandIn.bits.`type`, 2)
            poke(c.mutatorCommandIn.bits.length, 100)
            poke(c.mutatorCommandIn.bits.last, 1)
            poke(c.mutatorCommandIn.bits.id, 0)
            poke(c.mutatorCommandIn.valid, 1)

            poke(c.packetDetects.ready, 1)

            step(1)
            poke(c.mutatorCommandIn.bits.`type`, 1)
            step(1)
            poke(c.mutatorCommandIn.valid, 0)
            // poke(c.peakDetectConfig.numPeaks.valid, 0)

          }
        }
        poke(c.globalCycleEn, 1)
        poke(c.in.valid, 1)
        poke(c.out.ready, 1)

        for ((in, idx) <- signal.zipWithIndex) {
          poke(c.in.bits, in)
          if (idx == signal.length - 1) {
            poke(c.tlast, 1)
          }
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

  val stfParams = RXParams(protoADC = DspComplex(protoIn), protoAngle = protoAngle, protoFFTIn = DspComplex(protoOut),
    protoTwiddle = DspComplex(protoIn), protoLLR = protoIn, maxNumPeaks = 256, timeStampWidth = 64,
    autocorrParams = AutocorrParams(
        protoIn = DspComplex(protoIn),
        maxApart = 256,
        maxOverlap = 256
      ), ncoParams = NCOParams(
        phaseWidth = 16,
        tableSize = 64,
        phaseConv = u => u.asTypeOf(protoAngle),
        protoFreq = protoFreq,
        protoOut = protoOut,
      ), nFFT = 64)

  it should "correct no CFO with STF" in {
    val testSignal = IEEE80211.stf ++ IEEE80211.ltf ++ IEEE80211.sigFreq(5, 6 * 4, 0) ++ Seq.fill(500) { Complex(0.0, 0) }
    val cfoSignal = IEEE80211.addCFO(testSignal, 0.0e3)

    val output = runTest(cfoSignal, stfParams)

    println(s"Input was:")
    println(cfoSignal.toString)
    println(s"Output (length ${output.length}) was: $output")

}

  it should "correct CFO of 50 kHz with STF" in {
    val testSignal = IEEE80211.stf ++ Seq.fill(500) { Complex(0.0, 0) }
    val cfoSignal = IEEE80211.addCFO(testSignal, -50.0e3)

    val output = runTest(cfoSignal, stfParams)


    println(s"Input was:")
    println(cfoSignal.toString)
    println(s"Output (length ${output.length}) was: $output")
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

  it should "correct CFO for Rimas's test signal (two board)" ignore {
    val testSignal = ADITrace.binaryResource("/waveforms/wifi-bpsk-2boards.dat.xz").take(100000)
    val cfoSignal = testSignal //IEEE80211.addCFO(testSignal, -50.0e3)

    val output = runTest(cfoSignal, stfParams, thresh = 0.2)


    println(s"Input was:")
    println(cfoSignal.toString)
    println(s"Output was:")
    println(output.toString)
  }

}
