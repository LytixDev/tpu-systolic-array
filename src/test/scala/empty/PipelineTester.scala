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
          dut.io.inputIn.bits(i)(j).poke(input(i)(j).U)
        }
      }

      dut.io.inputIn.valid.poke(true.B)
      dut.io.outputOut.ready.poke(true.B)
      dut.clock.step(1)
      dut.io.inputIn.valid.poke(false.B)

      var cycles = 1
      while (!dut.io.outputOut.valid.peek().litToBoolean && cycles < 100) {
        dut.clock.step(1)
        cycles += 1
      }

      println(s"Computation took $cycles cycles")

      //assert(cycles == expectedCycles, s"Expected $expectedCycles cycles but got $cycles")
      dut.io.outputOut.bits(0)(0).expect(expected)
    }
  }


  "Pipeline" should "actually work being pipelined" in {
    // 4 layers, all 4x4 @ 4x4, all 1 PE per output (so should take 4 cycles each)
    // The idea is that once L1 is done with the first matmul it will be given a new one immediately afterwards
    // So we will have two inferences in-flight at various stages.
    // This actually tests if the whole pipeline is able to ... actually pipeline!

    val rand = new Random(42)
    var cycles = 0

    // Create 4 identical layers (for simplicity)
    // We use very small numbers for the weights and inputs because the quantization stuff is not implemented yet :-)
    val layers = Array.fill(4) {
      val weights = Array.fill(4, 4)(rand.nextInt(2))
      DenseLayer(m = 1, n = 4, k = 4, weights = weights, PEsPerOutput = 1)
    }

    val input1 = Array.fill(1, 4)(rand.nextInt(2))
    val input2 = Array.fill(1, 4)(rand.nextInt(2))

    def matmul(input: Array[Array[Int]], weights: Array[Array[Int]]): Array[Array[Int]] = {
      val m = input.length
      val n = input(0).length
      val k = weights(0).length

      val result = Array.ofDim[Int](m, k)
      for (i <- 0 until m) {
        for (j <- 0 until k) {
          var sum = 0
          for (idx <- 0 until n) {
            sum += input(i)(idx) * weights(idx)(j)
          }
          result(i)(j) = sum
        }
      }
      result
    }

    // Compute expected outputs by chaining matmuls through all 4 layers
    var expected1 = input1
    var expected2 = input2
    for (layer <- layers) {
      expected1 = matmul(expected1, layer.weights)
      expected2 = matmul(expected2, layer.weights)
    }

    // println(s"Expected output 1: ${expected1(0).mkString(", ")}")
    // println(s"Expected output 2: ${expected2(0).mkString(", ")}")

    test(new Pipeline(layers)).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>
      dut.io.outputOut.ready.poke(false.B)

      // Send first input
      for (i <- 0 until 1) {
        for (j <- 0 until 4) {
          dut.io.inputIn.bits(i)(j).poke(input1(i)(j).U)
        }
      }
      dut.io.inputIn.valid.poke(true.B)
      dut.clock.step(1)
      cycles += 1
      dut.io.inputIn.valid.poke(false.B)

      // Wait for first layer to become ready again (FIFO absorbed its output)
      var readyWait = 0
      while (!dut.io.inputIn.ready.peek().litToBoolean && readyWait < 20) {
        dut.clock.step(1)
        cycles += 1
        readyWait += 1
      }
      println(s"First layer ready again after $readyWait additional cycles")

      // Send second input now that layer is ready
      for (i <- 0 until 1) {
        for (j <- 0 until 4) {
          dut.io.inputIn.bits(i)(j).poke(input2(i)(j).U)
        }
      }
      dut.io.inputIn.valid.poke(true.B)
      dut.clock.step(1)
      cycles += 1
      dut.io.inputIn.valid.poke(false.B)

      // Wait for first output to become valid
      //var cycles = 2
      while (!dut.io.outputOut.valid.peek().litToBoolean && cycles < 100) {
        dut.clock.step(1)
        cycles += 1
      }

      println(s"First output arrived after $cycles cycles")

      // Capture first output
      val output1 = (0 until 4).map { j =>
        dut.io.outputOut.bits(0)(j).peek().litValue.toInt
      }.toArray
      println(s"Actual output 1:   ${output1.mkString(", ")}")

      // Verify first output matches expected
      for (j <- 0 until 4) {
        assert(output1(j) == expected1(0)(j),
          s"Output 1 mismatch at index $j: expected ${expected1(0)(j)}, got ${output1(j)}")
      }
      println("Output 1 matches expected!")

      // Accept first output
      dut.io.outputOut.ready.poke(true.B)
      dut.clock.step(1)

      // Wait for second output
      var cycles2 = 1
      while (!dut.io.outputOut.valid.peek().litToBoolean && cycles2 < 20) {
        dut.clock.step(1)
        cycles2 += 1
      }
      println(s"Second output arrived $cycles2 cycles after first")

      // Capture second output
      val output2 = (0 until 4).map { j =>
        dut.io.outputOut.bits(0)(j).peek().litValue.toInt
      }.toArray
      println(s"Actual output 2:   ${output2.mkString(", ")}")

      // Verify second output matches expected
      for (j <- 0 until 4) {
        assert(output2(j) == expected2(0)(j),
          s"Output 2 mismatch at index $j: expected ${expected2(0)(j)}, got ${output2(j)}")
      }
      println("✓ Output 2 matches expected!")

      println("Pipeline test passed: Both outputs are correct!")
    }
  }
}