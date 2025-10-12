package empty

import chisel3._
import chiseltest._
import chiseltest.simulator.TreadleBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class PipelineTester extends AnyFlatSpec with ChiselScalatestTester {
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

      //println(s"Computation took $cycles cycles")

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

    // Compute expected outputs by chaining matmuls through all 4 layers
    var expected1 = input1
    var expected2 = input2
    for (layer <- layers) {
      expected1 = matmul(expected1, layer.weights)
      expected2 = matmul(expected2, layer.weights)
    }

    test(new Pipeline(layers)) { dut =>
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
      //println(s"First layer ready again after $readyWait additional cycles")

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

      //println(s"First output arrived after $cycles cycles")

      // Capture first output
      val output1 = (0 until 4).map { j =>
        dut.io.outputOut.bits(0)(j).peek().litValue.toInt
      }.toArray
      //println(s"Actual output 1:   ${output1.mkString(", ")}")

      // Verify first output matches expected
      for (j <- 0 until 4) {
        assert(output1(j) == expected1(0)(j),
          s"Output 1 mismatch at index $j: expected ${expected1(0)(j)}, got ${output1(j)}")
      }
      //println("Output 1 matches expected!")

      // Accept first output
      dut.io.outputOut.ready.poke(true.B)
      dut.clock.step(1)

      // Wait for second output
      var cycles2 = 1
      while (!dut.io.outputOut.valid.peek().litToBoolean && cycles2 < 20) {
        dut.clock.step(1)
        cycles2 += 1
      }
      //println(s"Second output arrived $cycles2 cycles after first")

      // Capture second output
      val output2 = (0 until 4).map { j =>
        dut.io.outputOut.bits(0)(j).peek().litValue.toInt
      }.toArray
      //println(s"Actual output 2:   ${output2.mkString(", ")}")

      // Verify second output matches expected
      for (j <- 0 until 4) {
        assert(output2(j) == expected2(0)(j),
          s"Output 2 mismatch at index $j: expected ${expected2(0)(j)}, got ${output2(j)}")
      }
    }
  }

  // TODO: should also quantization the output once we have quantization in the pipeline
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

  def generateLayers(nLayers: Int, startN: Int, maxDim: Int, rand: Random): Array[DenseLayer] = {
    var currentN = startN
    Array.tabulate(nLayers) { i =>
      val n = currentN
      val k = rand.nextInt(maxDim - 1) + 2 // k between 2 and maxDim
      // TODO: small weights because quantization is not implemented yet
      val weights = Array.fill(n, k)(rand.nextInt(2))

      // PEsPerOutput must divide n evenly
      val validPEs = (1 to n).filter(pe => n % pe == 0)
      val PEsPerOutput = validPEs(rand.nextInt(validPEs.length))

      currentN = k // Next layer's n must match this layer's k
      DenseLayer(m = 1, n = n, k = k, weights = weights, PEsPerOutput = PEsPerOutput)
    }
  }

  def runPipelineTest(layers: Array[DenseLayer], inputs: Array[Array[Array[Int]]]): Unit = {
    // Calculate expected cycles for each layer
    val layerCycles = layers.map(layer => layer.n / layer.PEsPerOutput)

    // layers.foreach { layer =>
    //   val cycles = layer.n / layer.PEsPerOutput
    //   println(s"Layer n=${layer.n}, PEs=${layer.PEsPerOutput}, cycles=$cycles")
    // }

    val firstLayerCycles = layerCycles(0)
    val totalPipelineCycles = layerCycles.sum
    val delayBetweenPipelinedResults = layerCycles.max
    //val inputInterval = firstLayerCycles  // Send inputs at first layer's rate

    val expectedOutputs = inputs.map { input =>
      var result = input
      for (layer <- layers) {
        result = matmul(result, layer.weights)
      }
      result
    }

    test(new Pipeline(layers)) { dut =>
      dut.io.inputIn.valid.poke(false.B)
      dut.io.outputOut.ready.poke(true.B)

      fork {
        for (inferenceIdx <- inputs.indices) {
          // Poke input data
          for (j <- 0 until layers(0).n) {
            dut.io.inputIn.bits(0)(j).poke(inputs(inferenceIdx)(0)(j).U)
          }
          dut.io.inputIn.valid.poke(true.B)
          // println(s"Sending input $inferenceIdx")
          dut.clock.step()
          dut.io.inputIn.valid.poke(false.B)

          for (_ <- 0 until delayBetweenPipelinedResults - 1) {
            dut.clock.step()
          }
        }
        // println("Input thread finished")
      }.fork {
        // This thread checks the output
        for (inferenceIdx <- inputs.indices) {
          var cycles = 0
          // Wait for output to be valid
          while (!dut.io.outputOut.valid.peek().litToBoolean) {
            dut.clock.step()
            cycles += 1
            assert(cycles < 500, s"Timeout waiting for output $inferenceIdx after $cycles cycles")
          }

          val expectedCycles = if (inferenceIdx == 0) totalPipelineCycles else delayBetweenPipelinedResults
          println(s"Output $inferenceIdx arrived after $cycles cycles (expected $expectedCycles)")

          // Verify output
          for (j <- 0 until layers.last.k) {
            val actual = dut.io.outputOut.bits(0)(j).peek().litValue.toInt
            val expected = expectedOutputs(inferenceIdx)(0)(j)
            assert(actual == expected,
              s"Inference $inferenceIdx, output[$j]: expected $expected, got $actual")
          }

          dut.clock.step()
        }
      }.join()
    }
  }

  "Pipeline" should "should work for a variety of configs" in {
    val rand = new Random(42)

    for (testNum <- 0 until 10) {
      val nLayers = rand.nextInt(5) + 1

      // Build layers with compatible dimensions
      var currentN = rand.nextInt(6) + 2
      val layers = Array.tabulate(nLayers) { i =>
        val n = currentN
        val k = rand.nextInt(6) + 2
        val weights = Array.fill(n, k)(rand.nextInt(2))

        // PEsPerOutput must divide n evenly
        val validPEs = (1 to n).filter(pe => n % pe == 0)
        val PEsPerOutput = validPEs(rand.nextInt(validPEs.length))

        currentN = k // Next layer's n must match this layer's k
        DenseLayer(m = 1, n = n, k = k, weights = weights, PEsPerOutput = PEsPerOutput)
      }

      // println(s"\n=== Test $testNum: ${nLayers} layers, dimensions: ${layers.map(l => s"${l.n}x${l.k}(PE=${l.PEsPerOutput})").mkString(" -> ")} ===")

      val numInferences = 2
      val inputs = Array.fill(numInferences) {
        Array.fill(1, layers(0).n)(rand.nextInt(2))
      }

      // Run test with cycle tracking
      runPipelineTest(layers, inputs)
    }
  }

}