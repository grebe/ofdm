package ldpc

import chisel3.iotesters.PeekPokeTester

class EncoderTester(dut: Encoder, inOuts: Seq[(Seq[Boolean], Seq[Boolean])]) extends PeekPokeTester(dut) {
  poke(dut.in.valid, true)
  poke(dut.out.ready, true)

  for ( (in, out) <- inOuts) {
    dut.in.bits.zip(in).foreach { case (io, i) => poke(io, i) }
    dut.out.bits.zip(out).foreach { case (io, o) => expect(io, o) }
    step(1)
  }
}

object EncoderTester {
  def apply(generator: Seq[Seq[Boolean]], inOuts: Seq[(Seq[Boolean], Seq[Boolean])]): Boolean = {
    val params = LdpcParams(
      // bogus params, make an empty parity check matrix
      1,
      Seq.fill(generator.length)(Seq.fill(generator.head.length)(ZeroBlock(1))),
      generator = generator
    )
    chisel3.iotesters.Driver.execute(Array[String](), () => new Encoder(params)) {
      c => new EncoderTester(c, inOuts)
    }
  }
}