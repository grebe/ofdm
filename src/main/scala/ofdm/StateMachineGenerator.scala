package ofdm

import chisel3._
import chisel3.util.log2Ceil

case class StateMachineGeneratorIO(nStates: Int, inWidth: Int, outWidth: Int) extends Bundle {
  val wen = Input(Bool())
  val waddr = Input(UInt(log2Ceil(nStates).W))
  val wdata = Input(UInt(outWidth.W))

  val gotoStart = Input(Bool())

  val next = Input(Bool())
  val out  = Output(UInt(outWidth.W))
}

class StateMachineGenerator(nStates: Int, inWidth: Int, outWidth: Int) extends Module {
  val io = IO(StateMachineGeneratorIO(nStates = nStates, inWidth = inWidth, outWidth = outWidth))

  val mem = SyncReadMem(nStates, UInt(outWidth.W))

  val sAddr = RegInit(UInt(log2Ceil(nStates).W), 0.U)
  val sAddrNext = Wire(UInt())

  when (io.next) {

  }

  when (io.wen) {
    mem(io.waddr) := io.wdata
  }   .otherwise {
    io.out := mem(sAddrNext)
  }
}
