package empty

import chisel3._
import chisel3.util._

/*
 * Computes xW, where W is assumed to already be transposed, and x is rotated by 90.
 * Each PE in the first row is connected to a weight FIFO.
 * Each PE in the first column is connected to an input FIFO.
 * TODO:
 *       - Double buffering?
 *          - Load weights for next matmul while current matmul is still processing.
 *       - start the current matmul as early as possible even if the FIFOs are not completely filled
 *       - Test with tiling
 *          - What if the matmul we want to perform is smaller than the systolic array?
 *          - What if the matmul we want to perform is larger than the systolic array?
 */
class SystolicArrayGeneric(val rows: Int = 2, val cols: Int = 2, val weightFIFODepth: Int = 16,
                           val inputFIFODepth: Int = 16) extends Module {
  val io = IO(new Bundle {
    val weightIn = Input(Vec(cols, UInt(8.W)))
    val inputIn = Input(Vec(rows, UInt(8.W)))

    // control signals
    val loadWeight = Input(Vec(cols, Bool()))
    val loadInput = Input(Vec(rows, Bool()))
    val start = Input(Bool())
    //val reset = Input(Bool())
    //val resetPartialSum = Input(Bool())

    val accumulatorOut = Output(Vec(cols, UInt(32.W)))
  })

  // 8-bit data, 16 entries deep
  val weightFIFOs = VecInit(Seq.fill(cols)(Module(new Queue(UInt(8.W), weightFIFODepth)).io))
  val inputFIFOs  = VecInit(Seq.fill(rows)(Module(new Queue(UInt(8.W), inputFIFODepth)).io))

  // Staggering counters and control
  val cyclesSinceStart = RegInit(0.U(8.W))
  // How many weights have been sent to each column
  val weightCounters = RegInit(VecInit(Seq.fill(cols)(0.U(8.W))))

  val peArray2D = VecInit(Seq.fill(rows)(
    VecInit(Seq.fill(cols)(Module(new ProcessingElement()).io))
  ))

  when(io.start) {
    cyclesSinceStart := cyclesSinceStart + 1.U
  }

  // Connect FIFO control signals
  for (i <- 0 until rows) {
    inputFIFOs(i).enq.valid := io.loadInput(i)
    inputFIFOs(i).enq.bits := io.inputIn(i)

    // Inputs start after weights have filled the first column (rows cycles)
    // Then stagger: row i starts at cycle (rows + i)
    val inputStartCycle = rows.U + i.U
    val inputActive = io.start && (cyclesSinceStart >= inputStartCycle)
    inputFIFOs(i).deq.ready := inputActive
  }

  for (j <- 0 until cols) {
    weightFIFOs(j).enq.valid := io.loadWeight(j)
    weightFIFOs(j).enq.bits := io.weightIn(j)

    // Stagger weight FIFO dequeue: column j starts at cycle j
    // Stop after rows elements have been dequeued
    val weightStartCycle = j.U
    val weightActive = io.start && (cyclesSinceStart >= weightStartCycle) && (weightCounters(j) < rows.U)
    weightFIFOs(j).deq.ready := weightActive

    when(weightActive && weightFIFOs(j).deq.valid) {
      weightCounters(j) := weightCounters(j) + 1.U
    }
  }

  // Inputs and enable signals move to the right through rows
  for (i <- 0 until rows) {
    // First column gets inputs from FIFOs and enable signals
    peArray2D(i)(0).inputIn := inputFIFOs(i).deq.bits
    peArray2D(i)(0).enable := inputFIFOs(i).deq.valid && (cyclesSinceStart >= rows.U + i.U) && io.start

    // Subsequent columns get inputs and enable from previous PE
    for (j <- 0 until cols - 1) {
      peArray2D(i)(j + 1).inputIn := peArray2D(i)(j).inputOut
      peArray2D(i)(j + 1).enable := peArray2D(i)(j).enableOut
    }
  }

  for (j <- 0 until cols) {
    // PEs in the first row are connected to the FIFOs
    peArray2D(0)(j).weightIn := weightFIFOs(j).deq.bits

    // Accept weights from FIFO when it's ready and we haven't saturated yet
    val weightAccept = weightFIFOs(j).deq.valid && (weightCounters(j) < rows.U) && (cyclesSinceStart >= j.U) && io.start
    peArray2D(0)(j).acceptWeightIn := weightAccept

    peArray2D(0)(j).partialSumIn := 0.U

    // Weights and partial sums move down
    for (i <- 0 until rows - 1) {
      peArray2D(i + 1)(j).partialSumIn := peArray2D(i)(j).partialSumOut
      peArray2D(i + 1)(j).weightIn := peArray2D(i)(j).weightOut
      peArray2D(i + 1)(j).acceptWeightIn := weightAccept // peArray2D(i)(j).acceptWeightOut
    }
  }

  for (j <- 0 until cols) {
    io.accumulatorOut(j) := peArray2D(rows - 1)(j).partialSumOut
  }
}
