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

    val layer = DenseLayer(m = 2, n = 4, k = 2, weights = weights, PEsPerOutput = 2)

    test(new DenseDataflowFold(layer)) { dut =>
      // Set inputs (keep them stable for all cycles)
      for (i <- 0 until 2) {
        for (j <- 0 until 4) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      // Start computation
      dut.io.inputValid.poke(true.B)
      dut.clock.step(1)
      dut.io.inputValid.poke(false.B)

      // Wait for outputValid
      while (!dut.io.outputValid.peek().litToBoolean) {
        dut.clock.step(1)
      }

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

    val layer = DenseLayer(m = 2, n = 4, k = 2, weights = weights, PEsPerOutput = 1)

    test(new DenseDataflowFold(layer)) { dut =>
      for (i <- 0 until 2) {
        for (j <- 0 until 4) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      // Start computation
      dut.io.inputValid.poke(true.B)
      dut.clock.step(1)
      dut.io.inputValid.poke(false.B)

      // Wait for outputValid
      while (!dut.io.outputValid.peek().litToBoolean) {
        dut.clock.step(1)
      }

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

    val layer = DenseLayer(m = 2, n = 4, k = 2, weights = weights, PEsPerOutput = 4)

    test(new DenseDataflowFold(layer)) { dut =>
      for (i <- 0 until 2) {
        for (j <- 0 until 4) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      // Start computation
      dut.io.inputValid.poke(true.B)
      dut.clock.step(1)
      dut.io.inputValid.poke(false.B)

      // Wait for outputValid
      while (!dut.io.outputValid.peek().litToBoolean) {
        dut.clock.step(1)
      }

      for (i <- 0 until 2) {
        for (j <- 0 until 2) {
          dut.io.outputOut(i)(j).expect(expected(i)(j).U)
        }
      }
    }
  }
}
