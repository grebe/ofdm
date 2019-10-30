// Originally written for Fall 2018 EE290C modem class project
// Modified by Paul Rigge
package ofdm
package fft

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._
import breeze.numerics.{cos, sin}
import scala.math.Pi

sealed trait FFTType
case object DirectFFTType extends FFTType
case object SDFFFTType extends FFTType

sealed trait DecimType
case object DITDecimType extends DecimType
case object DIFDecimType extends DecimType
case object OptDecimType extends DecimType

/**
 * Base class for FFT parameters
 *
 * These are type generic
 */
case class FFTParams[T <: Data](
  numPoints    : Int,           // number of points in FFT
  protoIQ      : DspComplex[T], // input data type
  protoTwiddle : DspComplex[T], // twiddle data type
  fftType      : FFTType,       // type of FFT to use
  decimType    : DecimType,     // if SDF FFT is being used, whether to use DIT, DIF, or whatever is more "optimal"
  sdfRadix     : Int,           // if SDF FFT is being used, the radix
  pipeline     : Boolean,       // pipeline the fft
) {
  // Allowed values for some parameters
  final val allowedSDFRadices    = Seq(2, 4)

  // Common require functions used in FFT blocks
  def checkNumPointsPow2() {
    require(isPow2(numPoints), "number of points must be a power of 2")
  }
  def checkNumPointsPow() {
    require(FFTUtil.is_power(numPoints), "number of points must be a power of some number")
  }
  def checkSDFRadix() {
    require(allowedSDFRadices.contains(sdfRadix), s"""Radix must be one of the following: ${allowedSDFRadices.mkString(", ")}""")
  }
  def checkNumPointsPowOfSDFRadix() {
    require(FFTUtil.is_power_of(numPoints, sdfRadix), "number of points must be a power of the SDF radix")
  }
  def getPowerInfo(): (Int, Int) = {
    (FFTUtil.factorize(numPoints)._1.head, FFTUtil.factorize(numPoints)._2.head)
  }
}

object FFTParams {
  def fixed(dataWidth   : Int, // width of input and output
            binPoint    : Int, // binary point of input and output
            twiddleWidth: Int, // width of twiddle constants
            numPoints   : Int,
            pipeline    : Boolean = false,
            fftType     : FFTType = SDFFFTType,
            decimType   : DecimType = OptDecimType,
            sdfRadix    : Int = 2): FFTParams[FixedPoint] = {
    val protoIQ      = DspComplex(FixedPoint(dataWidth.W, binPoint.BP))
    // to allow for 1, -1, j, and -j to be expressed.
    val protoTwiddle = DspComplex(FixedPoint(twiddleWidth.W, (twiddleWidth-2).BP))
    FFTParams(
      numPoints = numPoints,
      protoIQ  = protoIQ,
      protoTwiddle = protoTwiddle,
      fftType = fftType,
      decimType = decimType,
      sdfRadix = sdfRadix,
      pipeline = pipeline
    )
  }
}

/**
 * Bundle type as IO for various blocks
 *
 * Many blocks use serial/parallel input and serial/parallel output streams
 */
// serial input, serial output
class SISOIO[T <: Data](proto: T) extends Bundle {
  val in = Flipped(Decoupled(proto))
  val out = Decoupled(proto)

  override def cloneType: this.type = SISOIO(proto).asInstanceOf[this.type]
}
object SISOIO {
  def apply[T <: Data](proto: T): SISOIO[T] = new SISOIO(proto)
}

// serial input, deserial output
class SIDOIO[T <: Data](len: Int, proto: T) extends Bundle {
  val in = Flipped(Decoupled(proto))
  val out = Decoupled(Vec(len, proto))

  override def cloneType: this.type = SIDOIO(len, proto).asInstanceOf[this.type]
}
object SIDOIO {
  def apply[T <: Data](len: Int, proto: T): SIDOIO[T] = new SIDOIO(len, proto)
}

// deserial input, serial output
class DISOIO[T <: Data](len: Int, proto: T) extends Bundle {
  val in = Flipped(Decoupled(Vec(len, proto)))
  val out = Decoupled(proto)

  override def cloneType: this.type = DISOIO(len, proto).asInstanceOf[this.type]
}
object DISOIO {
  def apply[T <: Data](len: Int, proto: T): DISOIO[T] = new DISOIO(len, proto)
}

// deserial input, deserial output
class DIDOIO[T <: Data](len: Int, proto: T) extends Bundle {
  val in = Flipped(Decoupled(Vec(len, proto)))
  val out = Decoupled(Vec(len, proto))

  override def cloneType: this.type = DIDOIO(len, proto).asInstanceOf[this.type]
}
object DIDOIO {
  def apply[T <: Data](len: Int, proto: T): DIDOIO[T] = new DIDOIO(len, proto)
}


/**
 * Top level FFT
 *
 * Instantiates the correct type of FFT based on parameter value
 */
class FFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(SIDOIO(params.numPoints, params.protoIQ))
  params.fftType match {
    case DirectFFTType => {
      // instantiate PacketDeserializer to go from serial-input FFT to parallel-input DirectFFT
      val deser = Module(new PacketDeserializer(PacketSerDesParams(params.protoIQ.cloneType, params.numPoints)))
      val fft   = Module(new DirectFFT(params))
      deser.io.in <> io.in
      fft.io.in   <> deser.io.out
      io.out      <> fft.io.out
    }
    case SDFFFTType => {
      val fft = Module(new SDFFFTDeserOut(params))
      fft.io.in  <> io.in
      fft.io.out <> io.out
    }
  }
}

/**
 * Top level IFFT
 *
 * Instantiates the correct type of FFT based on parameter value
 */
class IFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(DISOIO(params.numPoints, params.protoIQ))

  val fft_in  = Wire(io.in.cloneType)
  val fft_out = Wire(io.out.cloneType)

  // Bulk connect, but iq will be overridden in a following block of code
  fft_in  <> io.in
  fft_out <> io.out

  io.in.bits.zip(fft_in.bits).foreach {
    case (io_in, fft_inp) => {
      // Swap real and imaginary components
      fft_inp.real := io_in.imag
      fft_inp.imag := io_in.real
    }
  }

  val scalar = ConvertableTo[T].fromDouble(1.0 / params.numPoints.toDouble) // normalization factor
  // Swap real and imaginary components and normalize
  io.out.bits.real := fft_out.bits.imag * scalar
  io.out.bits.imag := fft_out.bits.real * scalar

  params.fftType match {
    case DirectFFTType => {
      // instantiate PacketSerializer to go from parallel-output DirectFFT to serial-output FFT
      val ser = Module(new PacketSerializer(PacketSerDesParams(params.protoIQ.cloneType, params.numPoints)))
      val fft = Module(new DirectFFT(params))
      fft_in    <> fft.io.in
      ser.io.in <> fft.io.out
      fft_out   <> ser.io.out
    }
    case SDFFFTType => {
      val fft = Module(new SDFFFTDeserIn(params))
      fft.io.in <> fft_in
      fft_out   <> fft.io.out
    }
  }
}

// Radix-n butterfly
object Butterfly {
  def apply[T <: Data : Real](in: Seq[DspComplex[T]]): Seq[DspComplex[T]] = {
    require(in.length == 2, "2-point DFT only for no defined twiddle type")
    Seq(in(0) + in(1), in(0) - in(1))
  }
  def apply[T <: Data : Real](in: Seq[DspComplex[T]], genTwiddle: DspComplex[T]): Seq[DspComplex[T]] = {
    in.length match {
      case 2 => apply(in)
      case _ => {
        val twiddlesSeq = (0 until in.length).map(n => {
          val twiddle_wire = Wire(genTwiddle.cloneType)
          twiddle_wire.real := Real[T].fromDouble( cos(2 * Pi / in.length * n))
          twiddle_wire.imag := Real[T].fromDouble(-sin(2 * Pi / in.length * n))
          twiddle_wire
        })
        Seq.tabulate(in.length)(k => {
          in.head + in.zipWithIndex.tail.map {
            case (inp, n) => inp * twiddlesSeq((k * n) % in.length)
          }.reduce(_ + _)
        })
      }
    }
  }
}
