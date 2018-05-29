package dsptools

import chisel3.iotesters.{PeekPokeTester, TesterOptionsManager}
import org.scalatest.{FlatSpec, Matchers}

class SyncROMBlackBoxTester(c: SyncROM) extends PeekPokeTester(c) {
  val max = BigInt(1) << c.addrWidth
  def getValueAtIdx(idx: BigInt): BigInt = {
    if (idx < c.table.length) {
      c.table(idx.toInt)
    } else {
      BigInt(0)
    }
  }

  // forwards
  var cnt = BigInt(0)

  while (cnt < max) {
    poke(c.io.addr, cnt)
    step(1)
    expect(c.io.data, getValueAtIdx(cnt))

    cnt += 1
  }

  // backwards
  cnt = max - 1
  while (cnt >= 0) {
    poke(c.io.addr, cnt)
    step(1)
    expect(c.io.data, getValueAtIdx(cnt))

    cnt -= 1
  }
}

class SyncROMSpec extends FlatSpec with Matchers {
  behavior of "SyncROM"

  val testTable:Seq[BigInt] = (0 until 2049).map(BigInt(_))

  it should "work with verilator" in {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new SyncROM("verilatorROM", testTable)) {
      c => new SyncROMBlackBoxTester(c)
    } should be (true)
  }

  it should "work with firrtl interpreter" ignore {
    val options = new TesterOptionsManager {
      interpreterOptions = interpreterOptions.copy(
        blackBoxFactories = interpreterOptions.blackBoxFactories :+ new SyncROMBlackBoxFactory
      )
    }
    chisel3.iotesters.Driver.execute(
      () => new SyncROM("firrtlROM", testTable),
      options) {
      c => new SyncROMBlackBoxTester(c)
    } should be (true)
  }

  it should "work with the interpreter with an increasing sequence" ignore {
    val options = new TesterOptionsManager {
      interpreterOptions = interpreterOptions.copy(
        blackBoxFactories = interpreterOptions.blackBoxFactories :+ new SyncROMBlackBoxFactory
      )
    }
    chisel3.iotesters.Driver.execute(
      () => new SyncROM("test_rom", table = (20 until 300).map(BigInt(_)) ),
      options
    ) { c =>
      new chisel3.iotesters.PeekPokeTester(c) {
        expect(c.io.data, 0)
        for (i <- 20 until 300) {
          poke(c.io.addr, i - 20)
          step(2)
          expect(c.io.data, i)
        }
      }
    } should be (true)
  }

}
