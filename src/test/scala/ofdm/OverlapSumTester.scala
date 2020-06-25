package ofdm

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.util.log2Ceil

import scala.collection._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class OverlapSumTester(c: OverlapSum[UInt], depth: Int) extends PeekPokeTester(c) {
  val width: Int = c.gen.getWidth

  var validIns : ArrayBuffer[BigInt] = mutable.ArrayBuffer[BigInt]()
  var validOuts: ArrayBuffer[BigInt] = mutable.ArrayBuffer[BigInt]()

  // set depth
  poke(c.io.depth.valid, 1)
  poke(c.io.depth.bits, depth)
  poke(c.io.in.valid, 0)
  step(1)

  poke(c.io.depth.valid, 0)
  expect(c.io.out.valid, 0)

  for (_ <- 0 until depth * 100) {
    // val in = i
    // val valid = true
    val in = BigInt(width - log2Ceil(depth), Random)
    val valid = Random.nextBoolean()

    if (valid) {
      validIns += in
    }
    poke(c.io.in.bits, in)
    poke(c.io.in.valid, valid)

    if (peek(c.io.out.valid) != BigInt(0)) {
      validOuts += peek(c.io.out.bits)
    }

    step(1)
  }

  val computedOuts = for (i <- 1 to validIns.length - depth) yield {
    val partialSumTerms = validIns.drop(i).take(depth)
    val partialSum = partialSumTerms.sum % (1L << width)
    partialSum
  }
  computedOuts.zip(validOuts).foreach { case (computed, seen) =>
    // println(s"Computed $computed but saw $seen")
    require(computed == seen, s"Computed $computed but saw $seen")
  }
}
