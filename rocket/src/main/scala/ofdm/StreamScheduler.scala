package ofdm

import chisel3._
import chisel3.util.{DecoupledIO, Queue, log2Ceil}
import dspblocks.{AXI4HasCSR, DspBlock, HasCSR, TLHasCSR}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream.{AXI4StreamMasterPortParameters, AXI4StreamNexusNode}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegReadFn, RegWriteFn}
import freechips.rocketchip.tilelink._

/**
  * Internal bundle type for stream scheduler
  * @param w
  */
class TimeAndLength(val w: Int) extends Bundle {
  val time = UInt(w.W)
  val length = UInt(w.W)
}

class SchedulingDescriptor(val w: Int, val numQueues: Int) extends Bundle {
  val tl = new TimeAndLength(w)
  val relative = Bool()
  val sel = UInt(log2Ceil(numQueues).W)
}

abstract class StreamScheduler[D, U, EO, EI, B <: Data](beatBytes: Int, counterOpt: Option[GlobalCycleCounter] = None)
  extends LazyModule()(Parameters.empty) with DspBlock[D, U, EO, EI, B] with HasCSR {

  override val streamNode = AXI4StreamNexusNode(
    masterFn = ms => ms.reduce({ (l, r) =>
        AXI4StreamMasterPortParameters(l.masters ++ r.masters)
    }),
    slaveFn = ss => {
      require(ss.length == 1, "Scheduler has one output")
      ss.head
    }
  )

  val hardCoded = BundleBridgeSink[DecoupledIO[SchedulingDescriptor]]()



  lazy val module = new LazyModuleImp(this) {
    val (ins, inPs) = streamNode.in.unzip
    require(streamNode.out.length == 1)
    val (out, outP) = streamNode.out.head

    val counter = counterOpt.getOrElse(new GlobalCycleCounter(beatBytes * 8, "StreamSchedulerCounter"))

    val len = ins.length
    val currentTime = counter()

    val queues = Seq.fill(len) { Module(new Queue(new TimeAndLength(beatBytes * 8), 8)) }

    out.valid := false.B

    for ((in, deq) <- ins zip queues.map(_.io.deq)) {
      val streamCounter = RegInit(0.U((beatBytes * 8).W))
      val go = deq.valid && deq.bits.time <= currentTime
      in.ready := out.ready && go
      when (go) {
        out.bits := in.bits
        out.valid := true.B
      }
      when (in.fire()) {
        streamCounter := streamCounter + 1.U
      }
      deq.ready := streamCounter === deq.bits.length
      when (deq.fire()) {
        streamCounter := 0.U
      }
    }

    val fields = queues.zipWithIndex.flatMap({ case (q, idx) =>
      val time = RegInit(0.U((beatBytes * 8).W))
      val length = RegInit(0.U((beatBytes * 8).W))
      q.io.enq.bits.time := time
      q.io.enq.bits.length := length
      q.io.enq.valid := false.B // default value, overridden later
      val hardCodedFiring = hardCoded.in.foldLeft(false.B) { case (firing, (io, _)) =>
          io.ready := !firing
          when (!firing && io.valid) {
            q.io.enq.bits.time := io.bits.tl.time + Mux(io.bits.relative, currentTime, 0.U)
            q.io.enq.bits.length := io.bits.tl.length
            q.io.enq.valid := true.B
          }
          firing || io.valid
      }
      Seq(
        RegField(beatBytes * 8, length, RegFieldDesc(name = s"length_$idx", desc = s"length for the ${idx}th queue")),
        RegField(beatBytes * 8, time, RegFieldDesc(name = s"time_$idx", desc = s"time for the ${idx}th queue")),
        RegField.r(beatBytes * 8, q.io.count,
          RegFieldDesc(name = s"count_$idx", desc = s"occupancy of the ${idx}th queue (max 8)")),
        RegField(
          1,
          r = RegReadFn((ivalid, oready) => (true.B, true.B, 0.U)), // iready, ovalid, data
          w = RegWriteFn((ivalid, _) => {
            when (!hardCodedFiring) {
              q.io.enq.valid := ivalid
            }
            !hardCodedFiring // true.B // iready
          }),
          desc = RegFieldDesc(name = s"go_$idx", desc = s"schedule stream $idx"))
      )
    })

    // call regmap with generated addresses
    regmap(fields.zipWithIndex.map({ case (f, idx) => idx * beatBytes -> Seq(f) }): _*)
  }
}

class AXI4_StreamScheduler(address: AddressSet, beatBytes: Int, counterOpt: Option[GlobalCycleCounter])
  extends StreamScheduler[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters,
    AXI4Bundle](beatBytes, counterOpt) with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes, concurrency = 1))
}

class TLStreamScheduler(address: AddressSet, beatBytes: Int, counterOpt: Option[GlobalCycleCounter])
  extends StreamScheduler[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn,
    TLBundle](beatBytes, counterOpt) with TLHasCSR {
  val dev = new SimpleDevice(devname = "bwrc,streamscheduler", devcompat = Seq("bwrc,streamscheduler"))
  val mem = Some(TLRegisterNode(address = Seq(address), device = dev, beatBytes = beatBytes, concurrency = 1))
}
