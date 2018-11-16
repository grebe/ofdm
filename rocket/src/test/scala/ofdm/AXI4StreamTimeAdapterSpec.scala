package ofdm

import chisel3._
import chisel3.iotesters.PeekPokeTester
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import org.scalatest.{FlatSpec, Matchers}

class AXI4StreamTimeAdapaterTestModule extends Module {

  implicit val p = Parameters.empty
  val lm = LazyModule(new LazyModule {
    val fuzzerADC  = AXI4StreamFuzzer(AXI4StreamTransaction.linearSeq(100))
    val fuzzerDAC  = AXI4StreamFuzzer(Seq.tabulate(100)(x => AXI4StreamTransaction(data = 100 + x, user = 100 + x)), u = 64)
    val (adc, dac) = AXI4StreamTimeAdapter()
    val adcOut     = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
    val dacOut     = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())

    adcOut := adc := fuzzerADC
    dacOut := dac := fuzzerDAC

    lazy val module = new LazyModuleImp(this) {
      val (a, aOutEdge) = adcOut.in.head
      val (d, dOutEdge) = dacOut.in.head
      val aOut = IO(AXI4StreamBundle(aOutEdge.bundle))
      val dOut = IO(AXI4StreamBundle(dOutEdge.bundle))
      aOut <> a
      dOut <> d
    }
  })
  val m = Module(lm.module)

  val io = IO(new Bundle {
    val adc = AXI4StreamBundle(m.aOutEdge.bundle)
    val dac = AXI4StreamBundle(m.dOutEdge.bundle)
  })

  io.adc <> m.aOut
  io.dac <> m.dOut
}

class AXI4StreamTimeAdapterTester(c: AXI4StreamTimeAdapaterTestModule)
  extends PeekPokeTester(c)
  with AXI4StreamSlaveModel {

  bindSlave(c.io.adc).addExpects(Seq.tabulate(100)(d => AXI4StreamTransactionExpect(data = Some(d))))
  bindSlave(c.io.dac).addExpects(Seq.tabulate(100)(x => AXI4StreamTransactionExpect(data = Some(100 + x))))

  stepToCompletion()
}

class AXI4StreamTimeAppenderSpec extends FlatSpec with Matchers {
  behavior of "AXI4StreamTimeAppender"

  it should "work with no DAC" in {
    chisel3.iotesters.Driver(() => new AXI4StreamTimeAdapaterTestModule) { new AXI4StreamTimeAdapterTester(_) } should be (true)
  }

  it should "work with multiple inputs to the DAC" ignore {

  }

  it should "work with multiple ADC inputs/outputs" ignore {

  }
}
