package ofdm

import chisel3._
import dsptools.numbers._

case class DSWPacketDetectConfig[T <: Data]
(
  autocorrConfig: AutocorrParams[T]
)

class DSWPacketDetectIO[T <: Data](params: DSWPacketDetectConfig[T]) extends AutocorrSimpleIO[T](params.autocorrConfig) {

}

class DSWPacketDetect[T <: Data : Ring](params: DSWPacketDetectConfig[T]) extends Module {
  val io = IO(new DSWPacketDetectIO(params))

  val autocorr = Module(new AutocorrSimple(params.autocorrConfig))

  autocorr.io.in     := io.in
  autocorr.io.config := io.config

}
