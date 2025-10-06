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

    // Send switch signal when the first input starts to flow
    dut.io.switchWeight.poke(true.B)

    // The inputs are flowing and after this loop the first result is ready
    for (i <- 0 until saRows - 1) {
      dut.clock.step()
    }
    dut.io.switchWeight.poke(false.B)
    dut.clock.step()

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

  "SystolicArrayGenericNew" should "just werk" in {
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

  "SystolicArrayGenericNew" should "be able to calculate two consecutive matmuls in a pipelined fashion" in {
    val saRows = 2
    val saCols = 2

    test(new SystolicArrayGenericNew).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // rows x columns
      val X1 = Array(
        Array(3, 6),
        Array(2, 2),
      )
      val X2 = Array(
        Array(4, 3),
        Array(2, 1),
      )

      val W_T1 = Array(
        Array(1, 2),
        Array(3, 4)
      )
      val W_T2 = Array(
        Array(6, 2),
        Array(8, 4)
      )

      val expect1 = matmul(X1, W_T1)
      val expect2 = matmul(X2, W_T2)

      val X1_in = matrixToColumns(X1)
      val X2_in = matrixToColumns(X2)
      val W_T1_in = matrixToColumnsReversed(W_T1)
      val W_T2_in = matrixToColumnsReversed(W_T2)


      dut.io.start.poke(false.B)
      dut.io.switchWeight.poke(false.B)

      // Enable loading for all FIFOs
      for (i <- 0 until saCols) {
        dut.io.loadWeight(i).poke(true.B)
      }
      for (i <- 0 until saRows) {
        dut.io.loadInput(i).poke(true.B)
      }

      // NOTE: This will pre-load all the data into the FIFOs before we start actually executing
      // Load all data into FIFOs: first matmul, then second matmul
      val maxLength1 = math.max(X1_in.map(_.length).max, W_T1_in.map(_.length).max)
      val maxLength2 = math.max(X2_in.map(_.length).max, W_T2_in.map(_.length).max)

      for (cycle <- 0 until maxLength1) {
        // Load first matmul inputs
        for (row <- X1_in.indices) {
          if (cycle < X1_in(row).length) {
            dut.io.inputIn(row).poke(X1_in(row)(cycle).U)
          }
        }
        // Load first matmul weights
        for (col <- W_T1_in.indices) {
          if (cycle < W_T1_in(col).length) {
            dut.io.weightIn(col).poke(W_T1_in(col)(cycle).U)
          }
        }
        dut.clock.step()
      }

      // Load second matmul data
      for (cycle <- 0 until maxLength2) {
        // Load second matmul inputs
        for (row <- X2_in.indices) {
          if (cycle < X2_in(row).length) {
            dut.io.inputIn(row).poke(X2_in(row)(cycle).U)
          }
        }
        // Load second matmul weights
        for (col <- W_T2_in.indices) {
          if (cycle < W_T2_in(col).length) {
            dut.io.weightIn(col).poke(W_T2_in(col)(cycle).U)
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


      /*
      * At this point the FIFOs are saturated with both X1, X2, W_T1, and W_T2.
      *
      * We will not start the SA computation.
      * The FIRST switch will be given when PE(0, 0) has gotten its final correct weight, I.E the entire first col
      * has been saturated. This is incidentally the same time the enable signal will be given to PE(0,0).
      * It is important that the SA continues to send weights after the first switch so that the "back" weight reg in
      * each PE is saturated with weights for the next matmul.
      *
      * NOTE: At some point handling the switch signals will be done by a control unit, but at this point we will do
      * it manually in the test.
      *
      * Subsequenet switches will happen at the same cycle the first input of a new matmul is sent to PE(0, 0).
      *
      * Since both input and weight matrices are 2x2 for both matmuls, weights and inputs will continue to stream
      * once the they have started.
      */

      dut.io.start.poke(true.B)


      val numberOfInputs = X1.length
      val totalCycles = saRows + saRows + (saCols * numberOfInputs) * 2

      fork {
        var cyclesSinceStart = 0
        // Control thread

        // Saturate the first column with first matmul weights.
        for (i <- 0 until saRows - 1) {
          dut.clock.step()
          cyclesSinceStart += 1
        }

        // Send initial switch signal one cycle before first input starts to flow
        dut.io.switchWeight.poke(true.B)
        dut.clock.step()
        cyclesSinceStart += 1
        dut.io.switchWeight.poke(false.B)

        // How many cycles until the next switch?
        // When PE(0,0) is completely done calculating partial sums for the first matmul
        // At this point, PE(0,0) has completed its first partial sum.
        // It will perform x1.cols partial sums for the first matmul. - 1 because of the step above.

        // The inputs are flowing
        for (i <- 0 until X1(0).length - 1) {
          dut.clock.step()
          cyclesSinceStart += 1
        }

        // Send second switch signal to start second matmul
        println(s"Sending second switch signal at cycle ${cyclesSinceStart} since start")
        dut.io.switchWeight.poke(true.B)
        dut.clock.step()
        cyclesSinceStart += 1
        dut.io.switchWeight.poke(false.B)

        // Continue clocking for remaining cycles
        val remainingCycles = X2.length + X2(0).length
        for (_ <- 0 until remainingCycles) {
          dut.clock.step()
        }
      }.fork {
        // Result checking thread
        for (cycle <- 0 until totalCycles) {
          val resultCheckCycle = cycle - (saRows + saRows)

          if (resultCheckCycle >= 0 && resultCheckCycle < saCols * numberOfInputs) {
            // Check first matmul results
            for (col <- 0 until expect1(0).length) {
              for (row <- 0 until expect1.length) {
                val expectedResultCycle = col + row
                if (resultCheckCycle == expectedResultCycle) {
                  val actual = dut.io.accumulatorOut(col).peek().litValue
                  val expected = expect1(row)(col)
                  println(s"Cycle $cycle: Matmul 1 at ($row, $col) - Expected $expected, Got $actual")
                  dut.io.accumulatorOut(col).expect(expect1(row)(col).U)
                }
              }
            }
          } else if (resultCheckCycle >= saCols * numberOfInputs) {
            // Check second matmul results
            val secondMatmulCycle = resultCheckCycle - (saCols * numberOfInputs)
            for (col <- 0 until expect2(0).length) {
              for (row <- 0 until expect2.length) {
                val expectedResultCycle = col + row
                if (secondMatmulCycle == expectedResultCycle) {
                  val actual = dut.io.accumulatorOut(col).peek().litValue
                  val expected = expect2(row)(col)
                  println(s"Cycle $cycle: Matmul 2 at ($row, $col) - Expected $expected, Got $actual")
                  dut.io.accumulatorOut(col).expect(expect2(row)(col).U)
                }
              }
            }
          }

          dut.clock.step()
        }
      }.join()
    }
  }


}