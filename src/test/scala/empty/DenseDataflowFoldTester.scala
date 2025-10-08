package empty

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DenseDataflowFoldTester extends AnyFlatSpec with ChiselScalatestTester {

  "DenseDataflowFold" should "compute 2x4 matrix multiplication with 2 PEs" in {
    val input = Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8)
    )
    val weights = Array(
      Array(1, 0),
      Array(0, 1),
      Array(1, 1),
      Array(1, 0)
    )

    // Should take 2 cycles with 2 PEs per output
    val expected = Array(
      Array(8, 5),
      Array(20, 13)
    )

    val layer = DenseLayer(m = 2, n = 4, k = 2, weights = weights)
    val numberOfPEs = 2

    test(new DenseDataflowFold(layer, numberOfPEs)) { dut =>
      // Set inputs (keep them stable for all cycles)
      for (i <- 0 until 2) {
        for (j <- 0 until 4) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      // Need to wait 2 cycles for computation
      dut.clock.step(2)

      // Check outputs
      for (i <- 0 until 2) {
        for (j <- 0 until 2) {
          dut.io.outputOut(i)(j).expect(expected(i)(j).U)
        }
      }
    }
  }

  "DenseDataflowFold" should "compute 2x4 matrix multiplication with 1 PE" in {
    val input = Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8)
    )

    val weights = Array(
      Array(1, 0),
      Array(0, 1),
      Array(1, 1),
      Array(1, 0)
    )

    val expected = Array(
      Array(8, 5),
      Array(20, 13)
    )

    val layer = DenseLayer(m = 2, n = 4, k = 2, weights = weights)
    val numberOfPEs = 1

    test(new DenseDataflowFold(layer, numberOfPEs)) { dut =>
      for (i <- 0 until 2) {
        for (j <- 0 until 4) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      // Should take 4 cycles now with only 1 PE per output neuron
      dut.clock.step(4)

      for (i <- 0 until 2) {
        for (j <- 0 until 2) {
          dut.io.outputOut(i)(j).expect(expected(i)(j).U)
        }
      }
    }
  }

  "DenseDataflowFold" should "compute 2x4 matrix multiplication with 4 PEs (no folding)" in {
    // With 4 PEs per output, should complete in 1 cycle (no folding)
    val input = Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8)
    )

    val weights = Array(
      Array(1, 0),
      Array(0, 1),
      Array(1, 1),
      Array(1, 0)
    )

    val expected = Array(
      Array(8, 5),
      Array(20, 13)
    )

    val layer = DenseLayer(m = 2, n = 4, k = 2, weights = weights)
    val numberOfPEs = 4

    test(new DenseDataflowFold(layer, numberOfPEs)) { dut =>
      for (i <- 0 until 2) {
        for (j <- 0 until 4) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      dut.clock.step(1)

      for (i <- 0 until 2) {
        for (j <- 0 until 2) {
          dut.io.outputOut(i)(j).expect(expected(i)(j).U)
        }
      }
    }
  }
}
