package ofdm

import breeze.numerics.sincpi
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.{Decoupled, ShiftRegister}
import dsptools.DspContext
import dsptools.numbers._

class SinglePointChannelEstimator[T <: Data : Ring : ConvertableTo](val params: RXParams[T]) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(SampleAndPilot(params.protoFFTOut, params.protoFFTOut))))
  val tlastIn = IO(Input(Bool()))

  val out = IO(Decoupled(params.protoChannelEst))
  val tlastOut = IO(Output(Bool()))

  val num = in.bits.pilot context_* in.bits.sample.conj()
  val numLatency = DspContext.current.numAddPipes * (if (DspContext.current.complexUse4Muls) 1 else 2)
    + DspContext.current.numMulPipes

  val magnitude: T = (in.bits.sample.real context_* in.bits.sample.real) context_+
                  (in.bits.sample.imag context_* in.bits.sample.imag)
  val magLatency = DspContext.current.numAddPipes + DspContext.current.numMulPipes

  val shiftAmt = params.protoChannelEst.real match {
    case f: FixedPoint => f.binaryPoint.get
    case _ => 0
  }
  val dividerWidth = magnitude.getWidth + shiftAmt
  val divider = Module(new PipelinedDivider(dividerWidth))
  divider.io.in.bits.num := convTtoUInt(ConvertableTo[T].fromDoubleWithFixedWidth(1.0, magnitude.cloneType)) << shiftAmt
  divider.io.in.bits.denom := convTtoUInt(magnitude)
  divider.io.in.valid := ShiftRegister(in.valid, magLatency, resetData = false.B, en = true.B)

  def convTtoUInt[U <: Data](in: U, shift: Boolean = false): UInt = in match {
    case f: FixedPoint => f.asUInt
    case r: DspReal => convTtoUInt(DspRealRealImpl.asFixed(r))
    case s: SInt => s.asUInt
    case u: UInt => u
  }
  def convUInttoT(in: UInt): T = in.asTypeOf(params.protoChannelEst.real)

  val numDelay = ShiftRegister(num, magLatency + divider.latency - numLatency)
  val mult = DspComplex.wire(
    numDelay.real context_* convUInttoT(divider.io.out.bits),
    numDelay.imag context_* convUInttoT(divider.io.out.bits)
  )

  val latency = magLatency + divider.latency + DspContext.defaultNumMulPipes

  println(s"latency=$latency")

  val outBuffering = PipeliningQueue(latency, in, out)
  outBuffering := mult

  tlastOut := ShiftRegister(tlastIn, latency, resetData = false.B, en = true.B)
}

class FlatPilotEstimator[T <: Data : Ring : ConvertableTo]
(val params: RXParams[T]) extends MultiIOModule {
  params.pilotPos.foreach (p => require(p >= 0 && p < params.nFFT, s"pilotPos must be in range [0, ${params.nFFT})"))
  params.pilotPos.sliding(2).foreach { case l :: r :: Nil => require(l < r, "pilotPos must be increasing") }

  val nPilots = params.pilotPos.length
  val maxPilotSeparation = (0 +: params.pilotPos :+ (params.nFFT - 1)).sliding(2).map(s => s(1) - s(0)).max

  val in = IO(Flipped(Decoupled(Vec(params.nFFT, params.protoFFTOut))))
  val pilots = IO(Input(Vec(nPilots, params.protoFFTIn)))
  val out = IO(Decoupled(Vec(params.nFFT, params.protoFFTOut)))

  def allButIdx(b: Seq[Bool], idx: Int): Bool = {
    require(idx >= 0 && idx < b.length)
    val withoutIdx = b.take(idx) ++ b.drop(idx + 1)
    TreeReduce(withoutIdx, (a: Bool, b: Bool) => a && b)
  }

  val estimators = Seq.fill(nPilots) { Module(new SinglePointChannelEstimator(params)) }
  val estReady = estimators.map(_.in.ready)
  val estValid = estimators.map(_.out.valid)

  val inDelayed = ShiftRegister(in.bits, estimators.head.latency)

  estimators.zip(pilots).foreach { case (est, pilot) => est.in.bits.pilot := pilot }
  estimators.zip(params.pilotPos).foreach { case (est, pos) => est.in.bits.sample := in.bits(pos) }
  estimators.zipWithIndex.foreach { case (est, idx) =>
      // all of these ready/valid signals should be simplified because they should be ready and valid at the same time
      // this is mostly defensive
      est.in.valid := in.valid && allButIdx(estReady, idx)
      est.out.ready := out.ready && allButIdx(estValid, idx)
  }
  estimators.foreach { case est => est.tlastIn := DontCare }

  in.ready := TreeReduce(estReady, (l: Bool, r: Bool) => l && r)
  out.valid := TreeReduce(estValid, (l: Bool, r: Bool) => l && r)

  // left edge - use the first est for every subcarrier
  for (i <- 0 until params.pilotPos.head) {
    out.bits(i) := inDelayed(i) * estimators.head.out.bits
  }
  // interpolate ests for the middle subcarriers
  for ((begin :: end :: Nil, pilotIdx) <- params.pilotPos.sliding(2).zipWithIndex) {
    val extent = end - begin
    val leftEst = estimators(pilotIdx).out.bits
    val rightEst = estimators(pilotIdx + 1).out.bits
    for (i <- 1 until extent) {
      val leftCoeff = ConvertableTo[T].fromDoubleWithFixedWidth(sincpi(i.toDouble / extent), params.protoFFTOut.real)
      val rightCoeff = ConvertableTo[T].fromDoubleWithFixedWidth(sincpi(1 - i.toDouble / extent), params.protoFFTOut.real)
      val left = DspComplex.wire(leftEst.real * leftCoeff, leftEst.imag * leftCoeff)
      val right = DspComplex.wire(rightEst.real * rightCoeff, rightEst.imag * rightCoeff)
      out.bits(i + begin) := inDelayed(i + begin) * (left + right)
    }
  }
  // right edge - use the last est for every subcarrier
  for (i <- params.pilotPos.last + 1 until params.nFFT) {
    out.bits(i) := inDelayed(i) * estimators.last.out.bits
  }
  // put zero through pilots
  for (p <- params.pilotPos) {
    out.bits(p).real := Ring[T].zero
    out.bits(p).imag := Ring[T].zero
  }
}