package ofdm

import chisel3._
import chisel3.internal.requireIsChiselType
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._

case class AutocorrParams[T <: Data]
(
  protoIn: T,
  maxApart: Int,
  maxOverlap: Int,
  // address: AddressSet,
  protoOut: Option[T] = None,
  name: String = "autocorr",
  base: Int = 0,
  beatBytes: Int = 4,
  addPipeDelay: Int = 3,
  mulPipeDelay: Int = 3
) {
  requireIsChiselType(protoIn,  s"genIn ($protoIn) must be chisel type")
  protoOut.foreach(g => requireIsChiselType(g, s"genOut ($g) must be chisel type"))
}

case class AutocorrConfigIO[T <: Data](params: AutocorrParams[T]) extends Bundle {
  val depthApart = Input(UInt(log2Ceil(params.maxApart+1).W))
  val depthOverlap = Input(UInt(log2Ceil(params.maxOverlap+1).W))
}

class AutocorrSimpleIO[T <: Data](params: AutocorrParams[DspComplex[T]]) extends Bundle {
  private val protoOut = params.protoOut.getOrElse(params.protoIn)
  val in = Flipped(Valid(params.protoIn))
  val out = Valid(protoOut)
  val energy = Valid(protoOut.real)

  val config = AutocorrConfigIO(params)
}

class AutocorrSimple[T <: Data : Ring](params: AutocorrParams[DspComplex[T]]) extends Module {
  // get fields from outer class
  val genIn      : DspComplex[T]   = params.protoIn
  val genOut     : DspComplex[T]   = params.protoOut.getOrElse(genIn)
  val shrMaxDepth: Int             = params.maxApart
  val maxOverlap : Int             = params.maxOverlap

  val io = IO(new AutocorrSimpleIO(params))

  // add delayed path to correlate with
  val shr = Module(new ShiftRegisterMem(genIn, shrMaxDepth))

  shr.io.depth.bits  := io.config.depthApart
  shr.io.depth.valid := io.config.depthApart =/= RegNext(io.config.depthApart)
  shr.io.in.valid    := io.in.fire()
  shr.io.in.bits     := io.in.bits

  val shrCount = RegInit(0.U(log2Ceil(shrMaxDepth + 1).W))

  when (shr.io.out.valid && shrCount < shrMaxDepth.U) {
    shrCount := shrCount +% 1.U
  }

  // correlate short and long path
  val toMult = shr.io.out.bits.conj()
  val prod   = io.in.bits * toMult
  assert(!(shrCount >= shrMaxDepth.U) || (io.in.fire() === shr.io.out.fire()),
    s"prod should always be a product of valid inputs")

  // sliding window
  val sum = Module(new OverlapSum(genOut, maxOverlap))

  sum.io.depth.bits  := io.config.depthOverlap
  sum.io.depth.valid := io.config.depthOverlap =/= RegNext(io.config.depthOverlap)

  // pipeline the multiply here
  sum.io.in.bits  := ShiftRegister(prod, params.mulPipeDelay)
  sum.io.in.valid := ShiftRegister(shrCount >= shrMaxDepth.U && io.in.fire(), params.mulPipeDelay,
                                   resetData = false.B, en = true.B)

  io.out.valid := sum.io.out.valid // && (shrCount >= shrMaxDepth.U)
  io.out.bits  := sum.io.out.bits

  val energySum = Module(new OverlapSum(genOut.real, maxOverlap))

  energySum.io.depth.bits  := io.config.depthOverlap
  energySum.io.depth.valid := io.config.depthOverlap =/= RegNext(io.config.depthOverlap)

  energySum.io.in.bits  := ShiftRegister(shr.io.out.bits.abssq(), params.mulPipeDelay)
  energySum.io.in.valid := ShiftRegister(io.in.fire() && shrCount >= shrMaxDepth.U, params.mulPipeDelay,
                                         resetData = false.B, en = true.B)

  assert(energySum.io.out.valid === sum.io.out.valid, "energySum and sum valid signals should be the same")
  io.energy.valid := energySum.io.out.valid
  io.energy.bits  := energySum.io.out.bits

  /**
    * Total delay in cycles from @io.in to @io.out and @io.energy
    */
  val totalDelay = params.mulPipeDelay + 1
}

object BuildSampleAutocorr {
  def main(args: Array[String]): Unit = {
    val params = AutocorrParams(
      DspComplex(FixedPoint(16.W, 14.BP), FixedPoint(16.W, 14.BP)),
      //DspComplex(FixedPoint(8.W, 4.BP), FixedPoint(8.W, 4.BP)),
      // genOut=Some(DspComplex(FixedPoint(16.W, 8.BP), FixedPoint(16.W, 8.BP))),
      maxApart = 32,
      maxOverlap = 32,
      beatBytes = 8)

    chisel3.Driver.execute(Array("-X", "verilog"), () => new AutocorrSimple(params))
  }
}
