package ofdm

import chisel3._
import chisel3.util._
import breeze.math.Complex
import chisel3.experimental.{ChiselEnum, chiselName}
import chisel3.internal.requireIsChiselType
import dsptools.{DspContext, DspTester}
import dsptools.numbers._

class FixedButterflyIO[T <: Data : Ring](genIn: T, genOut: T, n: Int) extends Bundle {
  val in       = Input (Vec(n, DspComplex(genIn, genIn)))
  val out      = Output(Vec(n, DspComplex(genOut, genOut)))
}

class FixedButterfly[T <: Data : Ring : ConvertableTo](genIn: T, genOut: T, genTwiddle: T, twiddles: Map[Int, Seq[Complex]], n: Int) extends Module {
  val io = IO(new FixedButterflyIO(genIn, genOut, n))

  for (outIdx <- 0 until n) {
    require(twiddles.contains(outIdx), s"Twiddles don't contain $outIdx")
    val out = io.out(outIdx)
    val t = twiddles(outIdx)
    require(t.length == n, s"Twiddles should be length $n, got ${t.length}")

    val terms = io.in.zip(t) map { case (input, twiddle) =>
      twiddle match {
        case Complex(0, 0) => None
        case Complex(1, 0) => Some(input)
        case Complex(-1, 0) => Some(-input)
        case Complex(0, 1) => Some(input.mulj())
        case Complex(0, -1) => Some(-input.mulj())
        case Complex(r, i) =>
          val real = ConvertableTo[T].fromDouble(r, genTwiddle)
          val imag = ConvertableTo[T].fromDouble(i, genTwiddle)
          Some(input * DspComplex(real, imag))
      }
    } collect { case Some(x) => x }

    require(terms.nonEmpty, s"Output $outIdx has no twiddles!")

    out := TreeReduce(terms, (x: DspComplex[T], y: DspComplex[T]) => x + y)
  }
}

class Butterfly2[T <: Data : Ring](genIn: T, genOut: Option[T] = None) extends MultiIOModule {
  val in0  = IO(Input (genIn.cloneType))
  val in1  = IO(Input (genIn.cloneType))

  val sum  = in0 + in1
  val diff = in0 - in1

  val out0 = IO(Output(genOut.getOrElse(sum).cloneType))
  val out1 = IO(Output(genOut.getOrElse(diff).cloneType))

  out0 := sum
  out1 := diff
}

class StreamingFFTIO[T <: Data : Ring](genIn: T, genOut: T) extends Bundle {
  val in   = Input (Valid(DspComplex(genIn, genIn)))
  val last = Input(Bool())
  val out  = Output(Valid(DspComplex(genOut, genOut)))
}

trait HasStreamingFFTIO[T <: Data] {
  def io: StreamingFFTIO[T]
}

object SimulateStreamingFFT {
  def apply[T <: Data, M <: Module with HasStreamingFFTIO[T]]
  (
    dut: () => M,
    in: Seq[Complex],
    expects: Option[Seq[Complex]] = None,
    tolerance: Int = 14
  ): Seq[Complex] = {
    var out: Seq[Complex] = Seq()
    var expectsMutable = expects.getOrElse(Seq())
    dsptools.Driver.execute( dut, Array[String]("-tbn", "verilator") ) { c => new DspTester(c) {
      def checkOutput(): Unit = {
        fixTolLSBs.withValue(tolerance) {
          if (peek(c.io.out.valid)) {
            out = out :+ peek(c.io.out.bits)
            if (expectsMutable.nonEmpty) {
              expect(c.io.out.bits, expectsMutable.head)
              expectsMutable = expectsMutable.tail
            }
          }
        }
      }
      poke(c.io.in.valid, 1)
      for (i <- in) {
        poke(c.io.in.bits, i)
        step(1)
        checkOutput()
      }
      for (_ <- in.indices) {
        step(1)
        checkOutput()
      }
    }}
    out
  }
}

class R2SDF[T <: Data : Ring : ConvertableTo](val n: Int, val genIn: T, val genOut: T, val genTwiddle: Option[T] = None)
  extends Module with HasStreamingFFTIO[T] {

  require(isPow2(n), s"Radix 2 FFT must be power of 2")

  val io = IO(new StreamingFFTIO(genIn, genOut))

  val nStages = log2Ceil(n)
  val delays = (1 to nStages).map { n >> _ }

  val en = io.in.valid

  val genTwiddleResolved = genTwiddle.getOrElse(genIn)

  io.out.bits := delays.foldLeft(io.in.bits) { case (in, delay) =>
    val bf = Module(new Butterfly2(in))

    val shifted = if (delay <= 1 || true) {
      ShiftRegister(bf.out0, delay, en = en)
    } else {
      val shr = Module(new ShiftRegisterMem(chiselTypeOf(in), delay))
      shr.io.in.valid := en
      shr.io.in.bits  := bf.out0
      shr.io.depth.valid := reset
      shr.io.depth.bits := delay.U
      shr.io.out.bits
    }

    bf.in0 := shifted
    if (delay == 1) {
      bf.in1 := in.mulj()
    } else {
      bf.in1 := in
    }

    val twiddlesComplex = Seq.tabulate(delay) { i => RootsOfUnity(i * n / delay, n) }
    val twiddlesDspComplex = twiddlesComplex.map( c => DspComplex.wire(
      ConvertableTo[T].fromDoubleWithFixedWidth(c.real, genTwiddleResolved),
      ConvertableTo[T].fromDoubleWithFixedWidth(c.imag, genTwiddleResolved)
    ))

    val twiddles = VecInit(twiddlesDspComplex)
    val (twiddleCount, _) = Counter(en, delay * 2)

    val product =  bf.out1 * twiddles(twiddleCount)
    val out = Wire(chiselTypeOf(product))

    when (twiddleCount < delay.U) {
      out := DspComplex.wire(Ring[T].zero, Ring[T].zero)
    } .otherwise {
      out := product
    }

    out
  }

  io.out.valid := ShiftRegister(io.in.valid, n, false.B, true.B)
}

class BF2I[T <: Data : Ring](proto: T, val delay: Int) extends MultiIOModule {
  requireIsChiselType(proto)
  require(delay > 0)

  val en = IO(Input(Bool()))
  val sel = IO(Input(Bool()))
  val in = IO(Input(DspComplex(proto)))
  val out = IO(Output(DspComplex(proto)))

  val feedbackIn = Wire(Valid(DspComplex(proto)))
  val feedback = if (delay > 1) { //delay > 1) {
    ShiftRegisterMem(feedbackIn, delay).bits
  } else {
    ShiftRegister(feedbackIn.bits, delay, en = feedbackIn.valid)
  }

  feedbackIn.valid := en

  when (sel) {
    feedbackIn.bits := feedback - in
  } .otherwise {
    // passthrough, swap
    feedbackIn.bits := in
  }
  when (ShiftRegister(sel, DspContext.current.numAddPipes)) { //, resetData = false.B, en = true.B)) {
    out := feedback context_+ in
  }.otherwise {
    out := ShiftRegister(feedback, DspContext.current.numAddPipes)
  }
}

class BF2II[T <: Data : Ring](proto: T, val delay: Int) extends MultiIOModule {
  requireIsChiselType(proto)
  require(delay > 0)

  val en = IO(Input(Bool()))
  val inMultByJ = IO(Input(Bool()))
  val sel = IO(Input(Bool()))
  val in = IO(Input(DspComplex(proto)))
  val out = IO(Output(DspComplex(proto)))

  val scaledIn = Wire(in.cloneType)
  scaledIn.real := in.imag
  scaledIn.imag := -in.real
  val inTwiddled = Mux(sel && inMultByJ, scaledIn, in)
  val feedbackIn = Wire(Valid(DspComplex(proto)))
  val feedback = if (delay > 1) { // (delay > 1) {
    ShiftRegisterMem(feedbackIn, delay).bits
  } else {
    ShiftRegister(feedbackIn.bits, delay, en = feedbackIn.valid)
  }

  feedbackIn.valid := en // ShiftRegister(en, DspContext.current.numAddPipes, resetData = false.B, en = true.B)
  when (sel) {
    feedbackIn.bits := feedback - inTwiddled
  } .otherwise {
    feedbackIn.bits := inTwiddled
  }
  when (ShiftRegister(sel, DspContext.current.numAddPipes)) { //, resetData = false.B, en = true.B)) {
    out := feedback context_+ inTwiddled
  }.otherwise {
    out := ShiftRegister(feedback, DspContext.current.numAddPipes)
  }
}

class R22Stage[T <: Data : Ring](proto:T, val n: Int) extends MultiIOModule {
  requireIsChiselType(proto)
  require(n > 2)
  require(isPow2(n))

  val en = IO(Input(Bool()))
  val in = IO(Input(DspComplex(proto)))
  val ctrl = IO(Input(UInt(2.W)))
  val protoOut = chiselTypeOf(in.real context_+ in.real)
  val out = IO(Output(DspComplex(protoOut)))

  val stage1 = Module(new BF2I(proto, n / 2))
  val stage2 = Module(new BF2II(protoOut, n / 4))

  stage1.in := in
  stage1.en := en
  stage1.sel := ctrl(1)

  stage2.in := stage1.out
  stage2.en := ShiftRegister(en, DspContext.current.numAddPipes, resetData = false.B, en = true.B)
  stage2.sel := ShiftRegister(ctrl(0), DspContext.current.numAddPipes)
  stage2.inMultByJ := ShiftRegister(!ctrl(1) && ctrl(0), DspContext.current.numAddPipes)

  out := stage2.out
}

class SDFInputManager[T <: Data](val n: Int, val proto: T) extends MultiIOModule {
  require(n > 0)
  requireIsChiselType(proto)

  val in = IO(Flipped(Decoupled(proto)))
  val in_last = IO(Input(Bool()))

  val out = IO(Decoupled(proto))
  val out_last = IO(Output(Bool()))

  val cnt = RegInit(0.U(log2Ceil(n).W))
  val last = RegInit(false.B)

  out <> in
  out_last := in_last && !last
  when (out.fire()) {
    cnt := cnt +% 1.U
  }

  when (in.fire() && in_last) {
    last := true.B
  }

  // this flushes the previous
  when (last) {
    out.bits := 0.U.asTypeOf(proto)
    out.valid := true.B
    in.ready := false.B
    when (out.fire() && cnt === (n - 1).U) {
      last := false.B
    }
  }
}

class SDFOutputManager[T <: Data](val n: Int, val proto: T) extends MultiIOModule {
  require(n > 0)
  requireIsChiselType(proto)

  val in = IO(Flipped(Decoupled(proto)))
  val in_last = IO(Input(Bool()))

  val out = IO(Decoupled(proto))
  val out_last = IO(Output(Bool()))

  object State extends ChiselEnum {
    // sLast is not the "last" state in the state machine,
    // it is the state you go to when in_last is asserted
    val sLast, sDrop, sPass = Value
  }

  val state = RegInit(State.sDrop)
  val cnt = RegInit(n.U)
  var drop = WireInit(false.B)

  when (in.fire() && cnt > 0.U) {
    cnt := cnt -% 1.U
  }


  // unless we are dropping, pass through
  out <> in
  when (drop) {
    in.ready := true.B
    out.valid := false.B
  }
  out_last := false.B

  when (state === State.sLast) {
    when (in.fire() && cnt === 0.U) {
      cnt := n.U
      state := State.sDrop
      out_last := true.B
    }
  }.elsewhen (state === State.sDrop) {
    drop := true.B
    when (in.fire() && cnt === 0.U) {
      state := State.sPass
    }
  } .elsewhen (state === State.sPass) {
    // stay in here until you see in_last
  }

  when (in.valid && in_last) {
    drop := false.B
    state := State.sLast
    cnt := n.U
  }
}

@chiselName
class R22SDF[T <: Data : Ring : ConvertableTo](val n: Int, val protoIn: T, val protoOut: T, val protoTwiddle: T)
extends MultiIOModule {
  requireIsChiselType(protoIn)
  requireIsChiselType(protoOut)
  requireIsChiselType(protoTwiddle)
  require(n > 2)
  require(isPow2(n))
  val logn = log2Ceil(n)
  require(logn % 2 == 0, "n must be a power of 4")

  val in = IO(Flipped(Decoupled(DspComplex(protoIn, protoIn))))
  val in_last = IO(Input(Bool()))
  val out = IO(Decoupled(DspComplex(protoOut, protoOut)))
  val out_last = IO(Output(Bool()))

  val inputManager = Module(new SDFInputManager(n - 1, DspComplex(protoIn, protoIn)))
  inputManager.in <> in
  inputManager.in_last := in_last
  val outputManager = Module(new SDFOutputManager(n - 1, DspComplex(protoOut, protoOut)))
  out <> outputManager.out
  out_last := outputManager.out_last

  val cnt = RegInit(0.U(logn.W))

  when (inputManager.out.fire()) {
    cnt := cnt +% 1.U
  }

  val complexMulLatency = DspContext.current.complexMulPipe

  //                input,         en,   last, ctrl, proto
  type StageInfo = (DspComplex[T], Bool, Bool, UInt, T)
  def makeStage(stageInfo: StageInfo, logStage: Int): StageInfo = {
    val (input, en, last, ctrl, proto) = stageInfo
    val stageN = 1 << (logn - logStage)
    val lastStage = stageN == 4
    val outputLatency = 2 * DspContext.current.numAddPipes + (if (lastStage) 0 else complexMulLatency)

    val stage = Module(new R22Stage(proto, stageN))
    stage.in := input
    stage.en := en
    stage.ctrl := ctrl.head(2)
    println(s"stage $stageN")

    val lastOut = if (!lastStage) {
      val twiddleIdxs =
        Seq.fill(stageN / 4)(0) ++
        Seq.tabulate(stageN / 4)(i => i * 2) ++
        Seq.tabulate(stageN / 4)(i => i) ++
        Seq.tabulate(stageN / 4)(i => i * 3) // ++
      val uniqueTwiddleIdxs = twiddleIdxs.distinct.sorted
      val uniqueTwiddleTable = VecInit(uniqueTwiddleIdxs.map(t =>
        DspComplex.wire(
          real = ConvertableTo[T].fromDoubleWithFixedWidth( math.cos(2 * math.Pi * t / stageN), protoTwiddle),
          imag = ConvertableTo[T].fromDoubleWithFixedWidth(-math.sin(2 * math.Pi * t / stageN), protoTwiddle)
        )))
      val twiddleIdxTable = VecInit(twiddleIdxs.map(i => {
        ( (uniqueTwiddleIdxs.indexOf(i) /*+ stageN / 2*/) /*% stageN*/).U
      }))

      val twiddleCnt = ShiftRegister(ctrl - (3 * n / 4).U, 2 * DspContext.current.numAddPipes)
      val twiddle = uniqueTwiddleTable(twiddleIdxTable(twiddleCnt))
      // ctrl +% (2 * DspContext.current.numAddPipes).U // - (stageN / 2 + 2 * DspContext.current.numAddPipes).U // ShiftRegister(stageCnt, 2 * DspContext.current.numAddPipes)
      stage.out context_* twiddle
    } else {
      stage.out
    }

    (
      lastOut,
      ShiftRegister(en, outputLatency, resetData = false.B, en = true.B),
      ShiftRegister(last, outputLatency),
      ShiftRegister(ctrl.tail(2), outputLatency, resetData = 0.U, en = true.B),
      chiselTypeOf(stage.out.real)
    )
  }

  val initStageInfo = (inputManager.out.bits, inputManager.out.fire(), inputManager.out_last, cnt, protoIn)
  val (outBits, outFire, outLast, outCnt, _) = (0 until logn by 2).foldLeft(initStageInfo)(makeStage)

  //             (1 add per BF stage)                     (1 mul every other stage, also excluding the last two)
  val latency = (DspContext.current.numAddPipes) * logn + complexMulLatency * ((logn - 2) / 2)
  println(s"latency = $latency, numAdd = ${DspContext.current.numAddPipes}, numMul = ${DspContext.current.numMulPipes}")
  // entries has + 1 because does not input does not flow through to output in single cycle
  val outQueue = Module(new Queue(DspComplex(protoOut), entries = latency + 1))
  val outLastQueue = Module(new Queue(Bool(), entries = latency + 1))
  outQueue.io.enq.bits := outBits
  outQueue.io.enq.valid := outFire
  assert(!outFire || outQueue.io.enq.ready, "Output dropped because output queue full")

  outLastQueue.io.enq.bits := outLast
  outLastQueue.io.enq.valid := outFire
  assert(!outFire || outLastQueue.io.enq.ready, "Last dropped because output queue full")
  assert(outQueue.io.deq.valid === outLastQueue.io.deq.valid)

  val inFlight = RegInit(0.U(log2Ceil(latency + 2).W))
  inFlight := inFlight +% inputManager.out.fire() -% outFire
  assert(inFlight > 0.U || inputManager.out.fire() || !outFire, "Underflow on the inFlight counter")

  inputManager.out.ready := inFlight +% outQueue.io.count < (latency + 1).U
  outputManager.in <> outQueue.io.deq
  outputManager.in_last := outLastQueue.io.deq.bits
  outLastQueue.io.deq.ready := outputManager.in.ready

  out <> outputManager.out
  out_last := outputManager.out_last
}
