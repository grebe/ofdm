package ofdm

import chisel3._
import chisel3.internal.requireIsChiselType
import chisel3.util.{ShiftRegister, Valid}

import dsptools.DspContext
import dsptools.numbers._

case class CFOCorrectParams[T <: Data]
(
  protoIn: DspComplex[T],
  nCOParams: NCOParams[T],
  protoOut: Option[DspComplex[T]] = None,
  mulPipe: Int = 3,
  addPipe: Int = 1
) {
  val getProtoOut = protoOut.getOrElse(protoIn)
}

class CFOCorrectIO[T <: Data](p: CFOCorrectParams[T]) extends Bundle {
  val in = Input(Valid(p.protoIn))
  val out = Output(Valid(p.getProtoOut))
  val freq = Input(p.nCOParams.protoFreq)
}

class CFOCorrect[T <: Data : Real : BinaryRepresentation](p: CFOCorrectParams[T]) extends Module {
  requireIsChiselType(p.protoIn)
  p.protoOut.foreach(requireIsChiselType(_))

  val io = IO(new CFOCorrectIO(p))

  val nco = Module(new NCO(p.nCOParams))

  nco.io.en := io.in.valid
  nco.io.freq := io.freq

  DspContext.withNumMulPipes(p.mulPipe) { DspContext.withNumAddPipes(p.addPipe)  {
    io.out.bits  := io.in.bits context_* nco.io.out.bits
  }}
  io.out.valid := ShiftRegister(io.in.valid, p.mulPipe + p.addPipe, resetData = false.B, en = true.B)
}
