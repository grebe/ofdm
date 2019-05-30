package ofdm
package fft

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

/**
 * Base classes for serializer and deserializer parameters
 *
 * These are type generic.
 * Note that there are 2 versions of these parameters.
 * One for the serializer/deserialization of PacketBundles, and another for BitsBundles.
 * TODO: With a better class hierarchy between PacketBundles and BitsBundles (as opposed to them being separate identities),
 * there would not be a need for two copies of these parameter classes, as well as the serializer and deserializer modules themselves.
 */
case class PacketSerDesParams[T <: Data]
(
  proto: T,
  ratio: Int // serialization/deserialization ratio
)

/**
  * Type specific case classes for the parameter base classes
  */
object PacketSerDesParams {
  def fixed(ratio: Int, dataWidth: Int, binPoint: Int): PacketSerDesParams[FixedPoint] = {
    PacketSerDesParams(FixedPoint(dataWidth.W, binPoint.BP), ratio)
  }

  def uint(ratio: Int, dataWidth: Int): PacketSerDesParams[UInt] = {
    PacketSerDesParams(UInt(dataWidth.W), ratio)
  }
}

/**
 * Bundle type as IO for PacketDeserializer modules
 */
class PacketDeserializerIO[T <: Data](params: PacketSerDesParams[T]) extends Bundle {
  val in  = Flipped(Decoupled(params.proto))
  val out = Decoupled(Vec(params.ratio, params.proto))

  override def cloneType: this.type = PacketDeserializerIO(params).asInstanceOf[this.type]
}
object PacketDeserializerIO {
  def apply[T <: Data](params: PacketSerDesParams[T]): PacketDeserializerIO[T] =
    new PacketDeserializerIO(params)
}

/**
 * Deserializer module.
 *
 * There's only one defined for deserializing PacketBundles because none is needed for BitsBundles.
 */
class PacketDeserializer[T <: Data](val params: PacketSerDesParams[T]) extends Module {
  val io = IO(PacketDeserializerIO(params))

  val sIdle :: sComp :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val state_next = Wire(state.cloneType)

  val cntr = RegInit(0.U(log2Up(params.ratio).W))
  val cntr_next = Wire(cntr.cloneType)

  // Default values
  cntr_next  := cntr
  state_next := state

  val deser = Reg(io.out.bits.cloneType)

  switch (state) {
    is (sIdle) {
      when (io.in.fire()) { state_next := sComp }
    }
    is (sComp) {
      when (io.in.fire()) {
        cntr_next      := cntr + 1.U
        // deser.pktStart := deser.pktStart || io.in.bits.pktStart
        // deser.pktEnd   := deser.pktEnd   || io.in.bits.pktEnd
        when (cntr === (params.ratio - 2).U) { state_next := sDone }
      }
    }
    is (sDone) {
      when (io.in.fire())       { state_next := sComp }
      .elsewhen (io.out.fire()) { state_next := sIdle }
    }
  }

  cntr  := cntr_next
  state := state_next

  when (state_next === sComp && state =/= sComp) {
    cntr_next      := 0.U
    // deser.pktStart := io.in.bits.pktStart
    // deser.pktEnd   := io.in.bits.pktEnd
  }

  // Shift register
  deser.foldRight(io.in.bits) {
    case (reg, inp) => {
      when (io.in.fire()) { reg := inp }
      reg
    }
  }

  io.in.ready  := !io.out.valid || io.out.ready // Deserializer can always receive more data unless there's backpressure
  io.out.valid := state === sDone
  io.out.bits  := deser
}

/**
 * Bundle type as IO for Serializer modules
 */
class PacketSerializerIO[T <: Data](params: PacketSerDesParams[T]) extends Bundle {
  val in  = Flipped(Decoupled(Vec(params.ratio, params.proto)))
  val out = Decoupled(params.proto)

  override def cloneType: this.type = PacketSerializerIO(params).asInstanceOf[this.type]
}
object PacketSerializerIO {
  def apply[T <: Data](params: PacketSerDesParams[T]): PacketSerializerIO[T] =
    new PacketSerializerIO(params)
}

/**
 * Serializer modules
 *
 * Two are defined, one for PacketBundle and another for BitsBundle.
 */
class PacketSerializer[T <: Data](val params: PacketSerDesParams[T]) extends Module {
  val io = IO(PacketSerializerIO(params))

  val sIdle :: sComp :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val in_flopped = Reg(io.in.bits.cloneType)

  val cntr = RegInit(0.U(log2Up(params.ratio).W))
  val cntr_next = Wire(cntr.cloneType)

  val serLast = cntr === (params.ratio - 1).U

  cntr_next := cntr

  when (state === sIdle) {
    when (io.in.fire()) {
      cntr_next := 0.U
      in_flopped := io.in.bits
      state := sComp
    }
  } .elsewhen (io.out.fire()) {
    when (serLast) {
      when (io.in.fire()) {
        cntr_next := 0.U
        in_flopped := io.in.bits
      } .otherwise {
        state := sIdle
      }
    } .otherwise {
      cntr_next := cntr + 1.U
    }
  }

  cntr := cntr_next

  io.out.bits := in_flopped(cntr)
  // io.out.bits.pktStart := in_flopped.pktStart && (cntr === 0.U)
  // io.out.bits.pktEnd := in_flopped.pktEnd && serLast
  io.out.valid := state === sComp
  // Serializer can receive more data if...
  // 1. idle
  // 2. done with current in_flopped
  io.in.ready := state === sIdle || (io.out.fire() && serLast)
}

class BitsSerializer[T <: Data](val params: PacketSerDesParams[T]) extends Module {
  val io = IO(PacketSerializerIO(params))

  val sIdle :: sComp :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val in_flopped = Reg(io.in.bits.cloneType)

  val cntr = RegInit(0.U(log2Up(params.ratio).W))
  val cntr_next = Wire(cntr.cloneType)

  val serLast = cntr === (params.ratio - 1).U

  cntr_next := cntr

  when (state === sIdle) {
    when (io.in.fire()) {
      cntr_next := 0.U
      in_flopped := io.in.bits
      state := sComp
    }
  } .elsewhen (io.out.fire()) {
    when (serLast) {
      when (io.in.fire()) {
        cntr_next := 0.U
        in_flopped := io.in.bits
      } .otherwise {
        state := sIdle
      }
    } .otherwise {
      cntr_next := cntr + 1.U
    }
  }

  cntr := cntr_next

  io.out.bits := in_flopped(cntr)

  // io.out.bits.pktStart := in_flopped.pktStart && (cntr === 0.U)
  // io.out.bits.pktEnd := in_flopped.pktEnd && serLast
  io.out.valid := state === sComp
  io.in.ready := state === sIdle || (io.out.fire() && serLast)
}

