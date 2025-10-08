package empty

import chisel3._
import chiseltest._
import chiseltest.simulator.TreadleBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class PipelineTester extends AnyFlatSpec with ChiselScalatestTester {

  /*
  "Pipeline" should "compute MNIST-like 2-layer network" in {
    // MNIST: 784 input pixels -> 32 hidden -> 10 output classes
    // Fully parallel
    val rand = new Random(42)

    val weights1 = Array.fill(784, 32)(rand.nextInt(256))
    val layer1 = DenseLayer(m = 1, n = 784, k = 32, weights = weights1, PEsPerOutput = 8)

    val weights2 = Array.fill(32, 10)(rand.nextInt(256))
    val layer2 = DenseLayer(m = 1, n = 32, k = 10, weights = weights2, PEsPerOutput = 8)

    val layers = Array(layer1, layer2)

    test(new Pipeline(layers)) { dut =>
      // dummy data
      val input = Array.fill(1, 784)(rand.nextInt(256))

      for (i <- 0 until 1) {
        for (j <- 0 until 784) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      dut.clock.step(1)

      // Not checking for correctness, just checking that it looks ait
      for (i <- 0 until 1) {
        for (j <- 0 until 10) {
          val output = dut.io.outputOut(i)(j).peek().litValue
          println(s"  Class $j: $output")
        }
      }
    }
  }
  */

  /*
  "Pipeline" should "just werk" in {
    // Fully parallel
    val rand = new Random(42)

    // 1x32 @ 32x8
    // 1x8 @ 8x4

    val weights1 = Array.fill(32, 8)(rand.nextInt(256))
    val layer1 = DenseLayer(m = 1, n = 32, k = 8, weights = weights1, PEsPerOutput = 32)

    val weights2 = Array.fill(8, 4)(rand.nextInt(256))
    val layer2 = DenseLayer(m = 1, n = 8, k = 4, weights = weights2, PEsPerOutput = 8)

    val layers = Array(layer1, layer2)

    test(new Pipeline(layers)).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>
      // dummy data
      val input = Array.fill(1, 32)(rand.nextInt(256))

      // Set inputs
      for (i <- 0 until 1) {
        for (j <- 0 until 32) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      // Assert inputValid to start computation
      dut.io.inputValid.poke(true.B)
      dut.clock.step(1)
      dut.io.inputValid.poke(false.B)

      // Wait for outputValid
      var cycles = 0
      while (!dut.io.outputValid.peek().litToBoolean && cycles < 100) {
        dut.clock.step(1)
        cycles += 1
      }

      println(s"Computation took $cycles cycles")

      // Check outputs
      for (i <- 0 until 1) {
        for (j <- 0 until 4) {
          val output = dut.io.outputOut(i)(j).peek().litValue
          println(s"  Class $j: $output")
        }
      }
    }
  }
  */

  "Pipeline" should "work with layers taking different amount of cycles" in {
    // Layer 1: 1x4 @ 4x2, with 1 PEs for each output (takes 4 cycles)
    // Layer 2: 1x2 @ 2x1, with 2 PEs for each output (takes 1 cycle)

    val input = Array(Array(1, 2, 3, 4))

    val weights1 = Array(
      Array(1, 0),
      Array(0, 1),
      Array(1, 1),
      Array(0, 1)
    )
    val weights2 = Array(
      Array(2),
      Array(3)
    )

    val expected = 35;
    val expectedCycles = 5;

    // area cost in terms of muls is the same for these two
    val layer1 = DenseLayer(m = 1, n = 4, k = 2, weights = weights1, PEsPerOutput = 1)
    val layer2 = DenseLayer(m = 1, n = 2, k = 1, weights = weights2, PEsPerOutput = 2)

    val layers = Array(layer1, layer2)

    test(new Pipeline(layers)).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>
      for (i <- 0 until 1) {
        for (j <- 0 until 4) {
          dut.io.inputIn(i)(j).poke(input(i)(j).U)
        }
      }

      dut.io.inputValid.poke(true.B)
      dut.clock.step(1)
      dut.io.inputValid.poke(false.B)

      var cycles = 1
      while (!dut.io.outputValid.peek().litToBoolean && cycles < 100) {
        dut.clock.step(1)
        cycles += 1
      }

      println(s"Computation took $cycles cycles")

      assert(cycles == expectedCycles, s"Expected $expectedCycles cycles but got $cycles")
      dut.io.outputOut(0)(0).expect(expected)
    }
  }
}