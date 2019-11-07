package ofdm

import chisel3._
import chisel3.experimental.MultiIOModule
import chisel3.util.{Decoupled, log2Ceil}
import dsptools.numbers._

class QPSKHardDemod[T <: Data : BinaryRepresentation](protoIn: T, n: Int) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(DspComplex(protoIn))))
  val out = IO(Decoupled(UInt(n.W)))
  val nchosen = IO(Input(UInt(log2Ceil(n).W)))

  for (bit <- 0 until n) {

  }

}

class QPSKDemod[T <: Data : Real](protoIn: T, protoLLR: T, n: Int) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(DspCompolex(proto))))
  val out = IO(Decoupled(Vec(n, protoLLR)))
  val nchosen = IO(Input(UInt(log2Ceil(n).W)))

}
