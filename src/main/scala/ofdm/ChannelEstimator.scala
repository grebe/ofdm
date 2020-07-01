package ofdm

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.{Decoupled, Queue, ShiftRegister, log2Ceil}
import dsptools.DspContext
import dsptools.numbers._

class SinglePointChannelEstimator[T <: Data : Ring : ConvertableTo](val params: RXParams[T]) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(SampleAndPilot(params.protoFFTOut, params.protoFFTOut))))
  val tlastIn = IO(Input(Bool()))

  val out = IO(Decoupled(params.protoChannelEst))
  val tlastOut = IO(Output(Bool()))

  val num = in.bits.pilot context_* in.bits.sample.conj()
  val numLatency = DspContext.current.complexMulPipe

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
  assert(
    ShiftRegister(in.valid, magLatency + divider.latency, resetData = false.B, en = true.B) ===
      divider.io.out.valid,
    "numerator and divider output must be valid at the same time"
  )
  val mult = DspComplex.wire(
    numDelay.real context_* convUInttoT(divider.io.out.bits),
    numDelay.imag context_* convUInttoT(divider.io.out.bits)
  )

  val latency = magLatency + divider.latency + DspContext.current.numMulPipes

  println(s"latency=$latency")

  val outBuffering = Skid(latency, in, out)
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
  val out = IO(Decoupled(Vec(params.nFFT, params.protoChannelEst)))

  val inQueue = Queue(in, entries = 1, flow = true)

  val estimator     = Module(new SinglePointChannelEstimator(params))
  val estimates     = Reg(Vec(nPilots, params.protoChannelEst))
  val pilotInCount  = RegInit(0.U(log2Ceil(nPilots + 1).W))
  val pilotOutCount  = RegInit(0.U(log2Ceil(nPilots + 1).W))
  val pilotIdxTable = VecInit(params.pilotPos.map(_.U))
  val pilotSample   = inQueue.bits(pilotIdxTable(pilotInCount))
  estimator.in.bits.pilot := pilots(pilotInCount)
  estimator.in.bits.sample := pilotSample
  estimator.in.valid := inQueue.valid
  val lastPilotIn = pilotInCount === (nPilots).U
  val lastPilotOut = pilotOutCount === (nPilots).U
  val outDone = RegInit(false.B)
  estimator.tlastIn := lastPilotIn

  inQueue.ready := lastPilotOut // lastPilotIn
  when (inQueue.valid && estimator.in.ready) {
    pilotInCount := Mux(lastPilotIn, 0.U, pilotInCount +% 1.U)
  }

  estimator.out.ready := !lastPilotOut || out.ready
  when (estimator.out.fire()) {
    estimates(pilotOutCount) := estimator.out.bits
    pilotOutCount := Mux(lastPilotOut, 0.U, pilotOutCount +% 1.U)
    outDone := lastPilotOut
  }

  val inDelayed = Module(new Queue(
    Vec(params.nFFT, params.protoFFTOut),
    estimator.latency + nPilots - 1))
  inDelayed.io.enq.bits := inQueue.bits
  inDelayed.io.enq.valid := inQueue.valid && estimator.in.ready && (pilotInCount === 0.U)

  out.valid := inDelayed.io.deq.valid && lastPilotOut
  inDelayed.io.deq.ready := out.ready && lastPilotOut
  when (out.fire()) {
    outDone := false.B
  }

  for ((o, idx) <- out.bits.zipWithIndex) {
    val pilotIdx = params.pilotPos.indexOf(idx)
    // it's not a pilot, it's data
    if (pilotIdx == -1) {
      o := inDelayed.io.deq.bits(idx)
    } else { // it's a pilot, put the estimate through
      o := estimates(pilotIdx)
    }
  }
}