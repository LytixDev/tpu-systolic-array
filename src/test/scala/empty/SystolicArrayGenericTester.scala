package empty

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SystolicArrayGenericTester extends AnyFlatSpec with ChiselScalatestTester {

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

  "SystolicArrayGeneric" should "just werk" in {
    test(new SystolicArrayGeneric).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // rows x columns
      // 1x2
      val X = Array(
        //Array(3, 1),
        Array(3, 6),
        Array(2, 2),
      )
      // 2x2
      val W_T = Array(
        Array(1, 2),
        Array(3, 4)
      )
      val expect = Array(
        Array(21, 30),
        // Array(8, 12),
      )

      // Vec(Vec(3), Vec(6))
      val X_in = matrixToColumns(X)
      // Vec(Vec(3, 1), Vec(4, 2))
      val W_in = matrixToColumnsReversed(W_T)

      // println(s"X_in: ${X_in.map(_.mkString("[", ", ", "]")).mkString("Array(", ", ", ")")}")
      // println(s"W_in: ${W_in.map(_.mkString("[", ", ", "]")).mkString("Array(", ", ", ")")}")

      // Initialize control signals
      dut.io.start.poke(false.B)

      // Enable loading for all FIFOs
      for (i <- 0 until 2) {
        dut.io.loadWeight(i).poke(true.B)
        dut.io.loadInput(i).poke(true.B)
      }

      // Load all data algorithmically
      val maxInputLength = X_in.map(_.length).max
      val maxWeightLength = W_in.map(_.length).max
      val maxLength = math.max(maxInputLength, maxWeightLength)

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
      for (i <- 0 until 2) {
        dut.io.loadWeight(i).poke(false.B)
        dut.io.loadInput(i).poke(false.B)
      }

      dut.io.start.poke(true.B)

      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()

      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
    }
  }
}