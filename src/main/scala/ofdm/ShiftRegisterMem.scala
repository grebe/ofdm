package ofdm

import chisel3._
import chisel3.internal.requireIsChiselType
import chisel3.util._

class ShiftRegisterMem[T <: Data](val gen: T, val maxDepth: Int) extends Module {
  require(maxDepth > 1, s"Depth must be > 1, got $maxDepth")
  requireIsChiselType(gen)

  val io = IO(new Bundle {
    val depth = Input(Valid(UInt(log2Ceil(maxDepth + 1).W)))
    val in    = Input(Valid(gen.cloneType))
    val out   = Output(Valid(gen.cloneType))
  })

  val mem         = SyncReadMem(maxDepth, gen)
  val readIdx     = Wire(UInt(log2Ceil(maxDepth).W))
  val readIdxReg  = RegInit(0.U(log2Ceil(maxDepth).W) - (maxDepth - 1).U)
  val writeIdxReg = RegInit(0.U(log2Ceil(maxDepth).W))

  when (io.depth.valid) {
    val diff = writeIdxReg - io.depth.bits + 1.U
    when (writeIdxReg + 1.U >= io.depth.bits) {
      readIdx := writeIdxReg - io.depth.bits + 1.U
    } .otherwise {
      readIdx := maxDepth.U + writeIdxReg + 1.U - io.depth.bits
    }
  } .otherwise {
    readIdx := readIdxReg
  }

  when (io.in.valid) {
    mem.write(writeIdxReg, io.in.bits)
    writeIdxReg := Mux(writeIdxReg < (maxDepth - 1).U, writeIdxReg + 1.U, 0.U)
  }

  val validPrev = RegNext(io.in.valid, init=false.B)
  when (io.in.valid) { //validPrev) {
    readIdxReg := Mux(readIdx < (maxDepth - 1).U, readIdx + 1.U, 0.U)
  } .otherwise {
    readIdxReg := readIdx
  }

  val outputQueue = Module(new Queue(gen, 1, pipe=true, flow=true))
  outputQueue.io.enq.valid := validPrev
  outputQueue.io.enq.bits := mem.read(readIdx)

  io.out.bits := outputQueue.io.deq.bits
  outputQueue.io.deq.ready := io.in.valid
  io.out.valid := io.in.valid
}

object ShiftRegisterMem {
  def apply[T <: Data](in: Valid[T], depth: Int): Valid[T] = {
    val depthW = Wire(Valid(UInt()))
    depthW.valid := RegNext(false.B, init=true.B)
    depthW.bits  := depth.U
    apply(in, depth, depthW)
  }

  def apply[T <: Data](in: Valid[T], maxDepth: Int, depth: Valid[UInt]): Valid[T] = {
    val shiftRegisterMem = Module(new ShiftRegisterMem(chiselTypeOf(in.bits), maxDepth))
    shiftRegisterMem.io.in := in
    shiftRegisterMem.io.depth := depth
    shiftRegisterMem.io.out
  }
}