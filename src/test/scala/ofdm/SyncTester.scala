package ofdm

import chisel3._
import dsptools.DspTester

class SyncTester[T <: Data](c: Sync[T]) extends DspTester(c) {

}
