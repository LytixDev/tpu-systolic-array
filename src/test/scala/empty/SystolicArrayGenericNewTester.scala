package empty

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SystolicArrayGenericNewTester extends AnyFlatSpec with ChiselScalatestTester {

  def extractColumn(matrix: Array[Array[Int]], colIndex: Int): Array[Int] = {
    matrix.map(row => row(colIndex))
  }

  def extractColumnReversed(matrix: Array[Array[Int]], colIndex: Int): Array[Int] = {
    matrix.map(row => row(colIndex)).reverse
  }

  def matrixToColumns(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    if (matrix.isEmpty || matrix(0).isEmpty) Array.empty
    else matrix(0).indices.map(colIndex => extractColumn(matrix, colIndex)).toArray
  }

  def matrixToColumnsReversed(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    if (matrix.isEmpty || matrix(0).isEmpty) Array.empty
    else matrix(0).indices.map(colIndex => extractColumnReversed(matrix, colIndex)).toArray
  }

  def matmul(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    val rowsA = a.length
    val colsA = a(0).length
    val colsB = b(0).length

    Array.tabulate(rowsA, colsB) { (i, j) =>
      (0 until colsA).map(k => a(i)(k) * b(k)(j)).sum
    }
  }

  def runSystolicArrayUntilFinish(X: Array[Array[Int]], W_T: Array[Array[Int]], dut: SystolicArrayGenericNew, saRows: Int = 2, saCols: Int = 2): Unit = {
    val expect = matmul(X, W_T);

    // Input to the FIFOs
    val X_in = matrixToColumns(X)
    val W_in = matrixToColumnsReversed(W_T)
    val maxInputLength = X_in.map(_.length).max
    val maxWeightLength = W_in.map(_.length).max
    val maxLength = math.max(maxInputLength, maxWeightLength)
    // println(s"X_in: ${X_in.map(_.mkString("[", ", ", "]")).mkString("Array(", ", ", ")")}")
    // println(s"W_in: ${W_in.map(_.mkString("[", ", ", "]")).mkString("Array(", ", ", ")")}")

    dut.io.start.poke(false.B)

    // Enable loading for all FIFOs
    for (i <- 0 until saCols) {
      dut.io.loadWeight(i).poke(true.B)
    }
    for (i <- 0 until saRows) {
      dut.io.loadInput(i).poke(true.B)
    }

    // Load all data into the FIFOs
    for (cycle <- 0 until maxLength) {
      // Load inputs if available
      for (row <- X_in.indices) {
        if (cycle < X_in(row).length) {
          dut.io.inputIn(row).poke(X_in(row)(cycle).U)
        }
      }
      // Load weights if available
      for (col <- W_in.indices) {
        if (cycle < W_in(col).length) {
          dut.io.weightIn(col).poke(W_in(col)(cycle).U)
        }
      }

      dut.clock.step()
    }

    // Stop loading
    for (i <- 0 until saCols) {
      dut.io.loadWeight(i).poke(false.B)
    }
    for (i <- 0 until saRows) {
      dut.io.loadInput(i).poke(false.B)
    }

    // Start the systolic array!
    dut.io.start.poke(true.B)

    // Saturate the first column. No inputs are flowing at this point.
    for (i <- 0 until saRows) {
      dut.clock.step()
    }
    // The inputs are flowing and after this loop the first result is ready
    for (i <- 0 until saRows) {
      dut.clock.step()
    }

    val numberOfInputs = X.length
    // The output is also staggered, so in the first cycle we only expect to see row(0)(0)
    // Then in the second cycle it should be row(1)(0), and row(0)(1), etc.
    for (i <- 0 until saCols * numberOfInputs) {
      // println(s"Regular: Index: $i, Accumulator 0: ${dut.io.accumulatorOut(0).peek().litValue}, Accumulator 1: ${dut.io.accumulatorOut(1).peek().litValue}")

      // TODO: I think this can be represented more cleanly
      // Calculate which results should be ready at this cycle
      // For each column and row in the output
      for (col <- 0 until expect(0).length) {
        for (row <- 0 until expect.length) {
          // Result for expect(row)(col) appears at cycle (col + row)
          val resultCycle = col + row
          if (i == resultCycle) {
            // println(s"Regular: Expecting result at index $i: expect($row)($col) = ${expect(row)(col)}")
            dut.io.accumulatorOut(col).expect(expect(row)(col).U)
          }
        }
      }

      dut.clock.step()
    }
  }

  def runSystolicArrayEarlyStart(X: Array[Array[Int]], W_T: Array[Array[Int]], dut: SystolicArrayGeneric, saRows: Int = 2, saCols: Int = 2): Unit = {
    val expect = matmul(X, W_T)

    // Input to the FIFOs
    val X_in = matrixToColumns(X)
    val W_in = matrixToColumnsReversed(W_T)
    val maxInputLength = X_in.map(_.length).max
    val maxWeightLength = W_in.map(_.length).max
    val maxLength = math.max(maxInputLength, maxWeightLength)

    dut.io.start.poke(false.B)

    // Enable loading for all FIFOs
    for (i <- 0 until saCols) {
      dut.io.loadWeight(i).poke(true.B)
    }
    for (i <- 0 until saRows) {
      dut.io.loadInput(i).poke(true.B)
    }

    // Load the first element into each FIFOs
    for (row <- X_in.indices) {
        dut.io.inputIn(row).poke(X_in(row)(0).U)
    }
    for (col <- W_in.indices) {
      dut.io.weightIn(col).poke(W_in(col)(0).U)
    }
    dut.clock.step()

    // Start the systolic array immediately after first elements are loaded
    dut.io.start.poke(true.B)

    // We will use the fork/join pattern which makes writing the test much easier
    // One thread will send in the data to the FIFOs while another one will check the final output for correctness
    val totalProcessingCycles = saRows + saRows + (saCols * X.length)

    fork {
      // Data loading thread
      for (cycle <- 1 until maxLength) {
        for (row <- X_in.indices) {
          if (cycle < X_in(row).length) {
            dut.io.inputIn(row).poke(X_in(row)(cycle).U)
          }
        }
        for (col <- W_in.indices) {
          if (cycle < W_in(col).length) {
            dut.io.weightIn(col).poke(W_in(col)(cycle).U)
          }
        }
        dut.clock.step()
      }

      for (i <- 0 until saCols) {
        dut.io.loadWeight(i).poke(false.B)
      }
      for (i <- 0 until saRows) {
        dut.io.loadInput(i).poke(false.B)
      }

      // Continue stepping clock for remaining processing.
      // TODO: Is this really needed?
      val remainingCycles = totalProcessingCycles - (maxLength - 1)
      for (_ <- 0 until remainingCycles) {
        dut.clock.step()
      }
    }.fork {
      // Result checking thread
      for (cycle <- 0 until totalProcessingCycles) {
        // Only check the results one the first result is ready
        val resultCheckCycle = cycle - (saRows + saRows)
        if (resultCheckCycle >= 0) {
          for (col <- 0 until expect(0).length) {
            for (row <- 0 until expect.length) {
              val expectedResultCycle = col + row
              if (resultCheckCycle == expectedResultCycle) {
                dut.io.accumulatorOut(col).expect(expect(row)(col).U)
              }
            }
          }
        }
        dut.clock.step()
      }
    }.join()
  }

  "SystolicArrayGeneric" should "just werk" in {
    test(new SystolicArrayGenericNew).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // rows x columns
      val X = Array(
        Array(3, 6),
        Array(2, 2),
      )
      val W_T = Array(
        Array(1, 2),
        Array(3, 4)
      )

      runSystolicArrayUntilFinish(X, W_T, dut)
    }
  }

}