package ofdm

import breeze.math.Complex

object RootsOfUnity {
  def apply(k: Int, n: Int): Complex = {
    Complex(math.E, 0.0).pow(Complex(0.0, -2*math.Pi*k/n))
  }

}
