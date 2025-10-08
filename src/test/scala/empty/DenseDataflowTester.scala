package empty

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DenseDataflowTester extends AnyFlatSpec with ChiselScalatestTester {

  "DenseDataflow" should "compute 2x2 matrix multiplication" in {
    val input = Array(
      Array(1, 2),
      Array(3, 4)
    )

    val weights = Array(
      Array(1, 0),
      Array(0, 1)
    )

    val expected = Array(
      Array(1, 2),
      Array(3, 4)
    )

    val layer = DenseLayer(m = 2, n = 2, k = 2, weights = weights)

    test(new DenseDataflow(layer)) { dut =>
      for (i <- 0 until 2) {
        for (j <- 0 until 2) {
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
