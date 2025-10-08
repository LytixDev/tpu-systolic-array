package empty

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DenseDataflowChainTester extends AnyFlatSpec with ChiselScalatestTester {

  "Two DenseDataflow layers" should "compute chained matrix multiplication" in {
    // Layer 1: input (2x3) * weights (3x4) = intermediate (2x4)
    // Layer 2: intermediate (2x4) * weights (4x2) = output (2x2)

    val input = Array(
      Array(1, 2, 3),
      Array(4, 5, 6)
    )

    val weights1 = Array(
      Array(1, 0, 1, 0),
      Array(0, 1, 0, 1),
      Array(1, 1, 0, 0)
    )

    val weights2 = Array(
      Array(1, 2),
      Array(3, 4),
      Array(5, 6),
      Array(7, 8)
    )

    val intermediate = Array(
      Array(4, 5, 1, 2),
      Array(10, 11, 4, 5)
    )

    val expected = Array(
      Array(38, 50),
      Array(98, 128)
    )

    val layer1 = DenseLayer(m = 2, n = 3, k = 4, weights = weights1, PEsPerOutput = 3)
    val layer2 = DenseLayer(m = 2, n = 4, k = 2, weights = weights2, PEsPerOutput = 4)

    // Module that connect multiple of these combinational layers into one
    test(new Module {
      val io = IO(new Bundle {
        val inputIn = Input(Vec(2, Vec(3, UInt(8.W))))
        val outputOut = Output(Vec(2, Vec(2, UInt(8.W))))
      })

      val denseLayer1 = Module(new DenseDataflow(layer1))
      val denseLayer2 = Module(new DenseDataflow(layer2))

      // Connect input to layer 1
      denseLayer1.io.inputIn := io.inputIn

      // Connect layer 1 output to layer 2 input
      denseLayer2.io.inputIn := denseLayer1.io.outputOut

      // Connect layer 2 output to module output
      io.outputOut := denseLayer2.io.outputOut
    }) { dut =>
      // Set inputs
      for (i <- 0 until 2) {
        for (j <- 0 until 3) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      dut.clock.step(1)

      // Check final output
      for (i <- 0 until 2) {
        for (j <- 0 until 2) {
          dut.io.outputOut(i)(j).expect(expected(i)(j).U)
        }
      }
    }
  }
}
