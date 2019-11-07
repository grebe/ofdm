package ofdm

import chisel3._
import chisel3.experimental.{FixedPoint, MultiIOModule}
import chisel3.util.{Decoupled, ShiftRegister}
import dsptools.DspContext
import dsptools.numbers._

class SinglePointChannelEstimator[T <: Data : Ring : ConvertableTo](params: RXParams[T]) extends MultiIOModule {
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

  val outBuffering = Skid(latency, in, out)
  outBuffering := mult

  tlastOut := ShiftRegister(tlastIn, latency, resetData = false.B, en = true.B)
}
