package channel

import breeze.math.Complex
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform}
import chisel3.internal.requireIsChiselType
import chisel3.util.experimental.loadMemoryFromFile
import chisel3.util._
import dsptools.numbers._
import firrtl.Transform
import firrtl.annotations.{Annotation, MemoryLoadFileType}
import firrtl.transforms.{BlackBoxInlineAnno, BlackBoxSourceHelper}
import freechips.rocketchip.amba.axi4stream.AXI4StreamIdentityNode
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import motion.{MotionIO, MotionSensorIO}
import ofdm.TreeReduce

object loadMemoryFromLiteral {
  def apply[T <: Data](memory: MemBase[T], data: Seq[T]): Unit = {
    require(memory.length == data.length, s"Data size (${data.length}) must match memory size (${memory.length})")
    val lits = for (d <- data) yield {
      d.litOption match {
        case Some(v) => v.toString(16)
        case None => require(false, "data must be literals")
      }
    }
    val litString = lits.mkString("\n")
    val fileName = memory.toNamed.name + "_init.hex"
    val anno = new ChiselAnnotation with RunFirrtlTransform {
      override def toFirrtl: BlackBoxInlineAnno = BlackBoxInlineAnno(memory.toNamed.module, fileName, litString)
      override def transformClass: Class[_ <: Transform] = classOf[BlackBoxSourceHelper]
    }
    loadMemoryFromFile(memory, fileName, hexOrBinary = MemoryLoadFileType.Hex)
  }
}

class MotionChannel[T <: Data : Ring : ConvertableTo](proto: T, nChannels: Int, taps: Seq[Seq[Complex]], clkFreqHz: Double,
                                                      distPerTapM: Double, maxVMS: Double)
  extends LazyModule()(Parameters.empty) {
  require(nChannels > 0)
  require(taps.length == nChannels)
  val nTaps = taps.head.length
  require(nTaps > 0)

  val posWidth: Int = 10 // TODO
  val velWidth: Int = 10
  val accWidth: Int = 10

  val channels = for (_ <- 0 until nChannels) yield AXI4StreamIdentityNode()
  val motions = for (_ <- 0 until nChannels) yield BundleBridgeSource(() => new MotionIO)

  override lazy val module = new LazyModuleImp(this) {
    // check that the channels are the right size
    for (c <- channels) {
      require(c.in.length == 1)
      require(c.out.length == 1)
      val p = c.in.head._2.bundle
      require(p.hasData)
      if (proto.isWidthKnown) {
        require(p.d * 8 >= proto.getWidth, s"channel is too small")
        require((p.d - 1) * 8 < proto.getWidth, s"channel is too big")
      }
    }

    /**
      * Make memorys holding channel taps.
      *
      * TODO describe memory layout
      */
    val mems = for (_ <- 0 until nChannels) yield SyncReadMem(nTaps, DspComplex(proto, proto))

    // set memory to be initialized with taps
    // we won't overwrite them, they are really just ROMs
    for ((m, t) <- mems.zip(taps)) {
      loadMemoryFromLiteral(m, t.map(tt => {
        val real = ConvertableTo[T].fromDouble(tt.real, proto)
        val imag = ConvertableTo[T].fromDouble(tt.imag, proto)
        (new DspComplex(proto, proto)).Lit(
          _.real -> real,
          _.imag -> imag,
        )
      }))
    }

    val sensorCnt = Counter(1024)

    // each user has a pos and vel register
    val pos = for ((_, i) <- motions.zipWithIndex) yield {
      val pos = RegInit(i.U(posWidth.W))
      pos
    }
    val vel = for ((_, i) <- motions.zipWithIndex) yield {
      val vel = RegInit(0.U(velWidth.W))
      vel
    }
    // function that makes logic to output sensor data
    // we need to be careful not too flood the sensor consumer with more data than it can consume, and don't want it to
    // eternally see very stale measurements. we make a single-entry queue that will hold the output and ignore new
    // output until the old measurement is consumed. a counter also limits the rate of output
    def makeHolder(sensorIO: DecoupledIO[MotionSensorIO], myPos: UInt, myVel: UInt, frontPos: UInt, backPos: UInt): Unit = {
      val sensorHolder = Module(new Queue(new MotionSensorIO, 1, pipe = false, flow = true))
      sensorHolder.io.deq <> sensorIO
      sensorHolder.io.enq.valid := sensorCnt.value === 0.U
      sensorHolder.io.enq.bits.frontDistance := frontPos - myPos
      sensorHolder.io.enq.bits.backDistance := myPos - backPos
      sensorHolder.io.enq.bits.velocity := myVel
    }

    // connect the output holders
    // special case the first and last sensors
    makeHolder(motions.head.bundle.sensor, myPos = pos.head, myVel = vel.head, frontPos = pos.head -% 1.U, backPos = pos.drop(1).head)
    makeHolder(motions.last.bundle.sensor, myPos = pos.last, myVel = vel.last, frontPos = pos.dropRight(1).last, backPos = pos.last -% 1.U)
    for (ms <- motions.zip(pos).zip(vel).sliding(3)) {
      val ((_,p1),_)::((m2,p2),v2)::((_,p3),_)::Nil = ms.toList
      // output current velocity and positions when sensorCnt is zero
      makeHolder(m2.bundle.sensor, myPos = p2, myVel = v2, frontPos = p1, backPos = p3)
    }

    // update position and velocity
    for (((m, p), v) <- motions.zip(pos).zip(vel)) {
      val acc = RegInit(0.U(accWidth.W))
      when (m.bundle.actuator.fire()) {
        acc := m.bundle.actuator.bits.acceleration
      }
      v := v +% acc // todo scaling, noise
      p := p +% v
    }

    // we model the channel every cycle, regardless of whether or not ready or valid are being driven
    //
    val ins = channels.map({c =>
      Mux(c.in.head._1.fire(), c.in.head._1.bits.data, 0.U).asTypeOf(DspComplex(proto, proto))
    })
    val outs = channels.map({c => c.out.head._1})

    for (i <- 0 until ins.length) {
      // compute the ith channel value to produce
      val taps = for ((m, j) <- mems.zipWithIndex) yield {
        m.read(pos(j))
      }
      // TODO make 1-entry queues
      outs(i).valid := true.B
      outs(i).bits := ins.map(x => RegNext(x)).zip(taps).map({ case (x, y) => x * y }).reduce(_ + _)
    }
  }
}
