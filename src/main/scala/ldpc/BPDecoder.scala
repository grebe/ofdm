package ldpc

import chisel3._
import chisel3.internal.{requireIsChiselType, requireIsHardware}
import chisel3.util.{Decoupled, ShiftRegister, isPow2, log2Ceil}
import dsptools.numbers._
import ofdm.TreeReduce

class VariableNode[T <: Data : Real](proto: T, protoAccum: T, accumCycles: Int) extends MultiIOModule {
  requireIsChiselType(proto)
  val c2vMin = IO(Input(proto))
  val c2v2ndMin = IO(Input(proto))
  val prior = IO(Input(proto))
  val firstIteration = IO(Input(Bool()))

  val decision = IO(Output(Bool()))
  val v2cMarg = IO(Output(proto))

  val accum = Reg(protoAccum)

  val v2cMargPrev = RegInit(proto, init = Ring[T].zero)

  val c2vMarg = Mux(c2vMin === v2cMargPrev, c2v2ndMin, c2vMin)
  val c2vMargDelay = ShiftRegister(c2vMarg, accumCycles)

  accum := Mux(firstIteration,
    prior,
    prior + c2vMarg
  )

  decision := accum.isSignNegative()
  v2cMarg := accum - c2vMargDelay
}

class CompareSwap[T <: Data : Order](proto: T) extends MultiIOModule {
  requireIsChiselType(proto)

  val in = IO(Input(Vec(2, proto)))
  val out = IO(Output(Vec(2, proto)))

  val comp = in(0) < in(1)

  out(0) := Mux(comp, in(0), in(1))
  out(1) := Mux(comp, in(1), in(0))
}

object CompareSwap {
  def apply[T <: Data : Order](in0: T, in1: T): (T, T) = {
    requireIsHardware(in0)
    requireIsHardware(in1)
    val compareSwap = Module(new CompareSwap(chiselTypeOf(in0)))
    compareSwap.in(0) := in0
    compareSwap.in(1) := in1
    (compareSwap.out(0), compareSwap.out(1))
  }
}

class SimpleBitonicLayer[T <: Data : Order](proto: T, val n: Int) extends  MultiIOModule {
  override def desiredName: String = s"SimpleBitonicLayer_$n"
  require(isPow2(n))
  require(n > 1)
  requireIsChiselType(proto)

  val in = IO(Input(Vec(n, proto)))
  val out = IO(Output(Vec(n, proto)))

  val mySwap = Wire(Vec(n, proto))
  for (i <- 0 until n / 2) {
    val topIn = i
    val bottomIn = i + n / 2
    val (topOut, bottomOut) = CompareSwap(in(topIn), in(bottomIn))
    mySwap(topIn) := topOut
    mySwap(bottomIn) := bottomOut
  }

  if (n > 2) {
    val nextTop = Module(new SimpleBitonicLayer(proto, n / 2))
    val nextBottom = Module(new SimpleBitonicLayer(proto, n / 2))

    for (i <- 0 until n / 2) {
      nextTop.in(i) := mySwap(i)
      nextBottom.in(i) := mySwap(i + n / 2)

      out(i) := nextTop.out(i)
      out(i + n / 2) := nextBottom.out(i)
    }
  } else {
    out := mySwap
  }
}

class SwappingBitonicLayer[T <: Data : Order](proto: T, val n: Int) extends MultiIOModule {
  override def desiredName: String = s"SwappingBitonicLayer_$n"
  require(isPow2(n))
  require(n > 1)
  requireIsChiselType(proto)

  val in = IO(Input(Vec(n, proto)))
  val out = IO(Output(Vec(n, proto)))

  val mySwap = Wire(Vec(n, proto))

  if (n > 4) {
    // add front layer
    val swappingTop = Module(new SwappingBitonicLayer(proto, n / 2))
    val swappingBottom = Module(new SwappingBitonicLayer(proto, n / 2))
    in.take(n / 2).zip(swappingTop.in).foreach { case (i, t) => t := i }
    in.drop(n / 2).zip(swappingBottom.in).foreach { case (i, b) => b := i }
    mySwap.take(n / 2).zip(swappingTop.in).foreach { case (m, t) => m := t }
    mySwap.drop(n / 2).zip(swappingBottom.out).foreach { case (m, b) => m := b }
  } else {
    // connect input directly to simple layers
    mySwap := in
  }

  if (n > 2) {
    val top = Module(new SimpleBitonicLayer(proto, n / 2))
    val bottom = Module(new SimpleBitonicLayer(proto, n / 2))

    mySwap.take(n / 2).zip(top.in).foreach { case (m, t) => t := m }
    mySwap.drop(n / 2).zip(bottom.in.reverse).foreach { case (m, b) => b := m }

    out.take(n / 2).zip(top.out).foreach { case (o, t) => o := t }
    out.drop(n / 2).zip(bottom.out.reverse).foreach { case (o, b) => o := b }
  } else {
    out := mySwap
  }
}

class BitonicSorter[T <: Data : Order](proto: T, val n: Int) extends MultiIOModule {
  override def desiredName: String = s"BitonicSorter_$n"
  require(isPow2(n))
  require(n > 1)
  requireIsChiselType(proto)

  val in = IO(Input(Vec(n, proto)))
  val out = IO(Output(Vec(n, proto)))

  if (n > 2) {
    val topSwapping = Module(new BitonicSorter(proto, n / 2))
    val bottomSwapping = Module(new BitonicSorter(proto, n / 2))
    in.take(n / 2).zip(topSwapping.in).foreach { case (i, t) => t := i }
    in.drop(n / 2).zip(bottomSwapping.in).foreach { case (i, b) => b := i }

    val topMerging = Module(new SimpleBitonicLayer(proto, n / 2))
    val bottomMerging = Module(new SimpleBitonicLayer(proto, n / 2))
    out.take(n / 2).zip(topMerging.out).foreach { case (o, t) => o := t }
    out.drop(n / 2).zip(bottomMerging.out).foreach { case (o, b) => o := b }

    for (i <- 0 until n / 2) {
      val (topOut, bottomOut) = CompareSwap(topSwapping.out(i), bottomSwapping.out(n / 2 - 1 - i))
      topMerging.in(i) := topOut
      bottomMerging.in(n / 2 - 1 - i) := bottomOut
    }
  } else {
    val (topOut, bottomOut) = CompareSwap(in(0), in(1))
    out(0) := topOut
    out(1) := bottomOut
  }
}

class CompareSelect[T <: Data : Real](proto: T, nIn: Int, nOut: Int) extends MultiIOModule {
  override def desiredName: String = s"CompareSelect_in${nIn}_out$nOut"
  requireIsChiselType(proto)
  require(nIn > 0)
  require(nOut > 0 && nOut <= nIn)

  val in = IO(Input(Vec(nIn, proto)))
  val out = IO(Output(Vec(nOut, proto)))

  val sortSize = if (nIn > 1) 1 << log2Ceil(nIn) else 2 // need at least two to sort

  val sorter = Module(new BitonicSorter(proto, sortSize))
  sorter.in.zip(in).foreach { case (s, i) => s := i }
  sorter.in.drop(in.length).foreach { case s => s := Ring[T].zero } // TODO, should be a minimum

  sorter.out.zip(out).foreach { case (s, o) => o := s }
}

class CheckNode[T <: Data : Real](proto: T, n: Int) extends MultiIOModule {
  requireIsChiselType(proto)
  require(n > 1)

  val v2c = IO(Input(Vec(n, proto)))
  val c2vSign = IO(Output(Bool()))
  val c2vMin = IO(Output(proto))
  val c2v2ndMin = IO(Output(proto))

  val v2cSigns: Seq[Bool] = v2c.map(_.isSignNegative())
  val v2cMags: Seq[T] = v2c.map(_.abs())

  val xor = (x: Bool, y: Bool) => { x ^ y }
  c2vSign := TreeReduce(v2cSigns, xor)

  val cs = Module(new CompareSelect(proto, nIn = n, nOut = 2))

  cs.in.zip(v2cMags).foreach { case (in, mag) => in := mag }
  c2vMin := cs.out(0)
  c2v2ndMin := cs.out(1)
}

class Shifter[T <: Data](proto: T, val n: Int) extends MultiIOModule {
  val in = IO(Input(Vec(n, proto)))
  val shift = IO(Input(UInt(log2Ceil(n).W)))
  val out = IO(Output(Vec(n, proto)))

  def stage(l: Seq[T], idx: Int): Seq[T] = {
    for (i <- 0 until n) yield {
      val left = i
      val right = (i + (1 << idx)) % n
      Mux(shift(idx), l(right), l(left))
    }
  }

  def allStages(l: Seq[T], idx: Int): Seq[T] = {
    if (idx < 0) {
      l
    } else {
      allStages(stage(l, idx), idx - 1)
    }
  }

  out.zip(allStages(in, log2Ceil(n) - 1)).foreach { case (o, s) => o := s }
}

class BPDecoder[T <: Data : Real](protoLLR: T, val params: LdpcParams) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(Vec(params.n, protoLLR))))
  val out = IO(Decoupled(Vec(params.k, Bool())))

  val schedule = params.schedule

  // state machine
  val fullIters = RegInit(0.U(64.W)) // log2Ceil(params.maxIters).W))
  val iterCount = RegInit(0.U(64.W)) // log2Ceil(params.columnDegree)))
  val sIdle :: sWorking :: sOutput :: Nil = chisel3.util.Enum(3)
  val state = RegInit(sIdle)

  val checkNodes = Seq.fill(params.blockSize)(Module(new CheckNode(protoLLR, n = schedule.head.entries.length)))
  val variableNodes = Seq.fill(params.n)(
    Module(new VariableNode(protoLLR, protoAccum = protoLLR, accumCycles = schedule.length)))

  variableNodes.zip(in.bits).foreach { case (v, llr) =>
    v.prior := llr
  }
  for (v <- variableNodes) {
    v.firstIteration := fullIters === 0.U && iterCount === 0.U
  }
  variableNodes.zip(out.bits).foreach { case (v, out) =>
      out := v.decision
  }

  val vng = variableNodes.grouped(params.blockSize).toSeq

  // the result of this expression is a Seq[] of wires.
  // each entry in the Seq is a tuple with three entries:
  //  1) Vec[Vec[T]] of inputs to VNGs
  //  2) Vec[T] of outputs of VNGs
  val (shiftIns, shiftOuts) = (for ((group, idx) <- vng.zipWithIndex) yield {
    val mySchedule = schedule.map(_.entries(idx))
    val backShift = Module(new Shifter(Vec(2, protoLLR), params.blockSize))
    val frontShift = Module(new Shifter(protoLLR, params.blockSize))

    require(mySchedule.map(_._1.enabled).reduce(_ && _), "Currently don't support holes in the schedule")

    val shiftTable = VecInit(mySchedule.map(_._1.shift.U))
    backShift.shift := shiftTable(iterCount)
    frontShift.shift := (params.blockSize - 1).U - shiftTable(iterCount)

    backShift.out.zip(group).foreach { case (i, g) =>
      g.c2vMin := i(0)
      g.c2v2ndMin := i(1)
    }
    frontShift.in.zip(group).foreach { case (o, g) =>
        o := g.v2cMarg
    }

    (backShift.in, frontShift.out)
  }).unzip

  // variable to check
  for (block <- 0 until params.blockSize) {
    for ((varNode: Seq[T], groupIdx) <- shiftOuts.zipWithIndex) {
      checkNodes(block).v2c(groupIdx) := varNode(block)
    }
  }

  def convertSign(sign: Bool, abs: T): T = {
    Mux(sign, -abs, abs)
  }
  // check -> variable
  val c2vs = for (block <- 0 until params.blockSize) yield {
    val min = convertSign(checkNodes(block).c2vSign, checkNodes(block).c2vMin)
    val min2 = convertSign(checkNodes(block).c2vSign, checkNodes(block).c2v2ndMin)
    VecInit(min, min2)
  }
  // connect check -> variable
  for ((sh, i) <- shiftIns.zipWithIndex) {
    val mySchedule = schedule.map(_.entries(i))
    val inputIdxs = mySchedule.map(_._2)
    val uniqueInputIdxs = inputIdxs.distinct
    val inputTable = VecInit(inputIdxs.map(i => uniqueInputIdxs.indexOf(i).U))
    val inputs = VecInit(uniqueInputIdxs.map(i => checkNodes(i)).map(n => VecInit(n.c2vMin, n.c2v2ndMin)))
     for (j <- 0 until params.blockSize) {
      sh(j) := inputs(inputTable(iterCount))
    }
  }

  val fullIterEnd = iterCount === (params.columnDegree - 1).U
  val iterDone = fullIters >= (params.maxIters - 1).U && fullIterEnd
  
  in.ready := false.B
  out.valid := iterDone
  when (state === sIdle && in.valid) {
    state := sWorking
    iterCount := 0.U
    fullIters := 0.U
  }

  when (state === sWorking) {
    iterCount := Mux(fullIterEnd, 0.U, iterCount +% 1.U)
    fullIters := Mux(fullIterEnd, fullIters +% 1.U, fullIters)
    when (iterDone) {
      state := sOutput
    }
  }

  when (iterDone && out.ready) {
    in.ready := true.B
    state := sIdle
    iterCount := 0.U
    fullIters := 0.U
  }
}
