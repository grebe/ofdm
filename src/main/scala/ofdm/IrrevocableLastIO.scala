package ofdm

import chisel3._
import chisel3.util._

class IrrevocableLastIO[+T <: Data](gen: T) extends IrrevocableIO(gen) {
  val last = Output(Bool())

  def fireLast(): Bool = this.fire() && last
}

object IrrevocableLast {
  def apply[T <: Data](gen: T): IrrevocableLastIO[T] = new IrrevocableLastIO(gen)
}
