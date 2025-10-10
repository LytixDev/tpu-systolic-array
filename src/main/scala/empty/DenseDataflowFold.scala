package empty

import chisel3._
import chisel3.util.{log2Ceil, Decoupled, Queue}

/*
 * Dense layer with folding
 * NOTE: Assumes 8 bit quantization of inputs and weights with 32 bit accumulators
 *
 * numberOfPEs = number of multipliers per output channel (per k)
 * Total multipliers = m * k * numberOfPEs
 * Takes n / numberOfPEs cycles to compute
 *
 * Uses Decoupled (ready/valid) interfaces for flow control between layers.
 * -> Similar to FINN, altough they use AXI-streams which we should perhaps look into as well
 *
 * NOTE: Each layer make the assumtion that the downstream FIFO always has room to push a new layers' outputs to.
 */
class DenseDataflowFold(layer: DenseLayer, inFifoDepth: Int = 2) extends Module {
  val io = IO(new Bundle{
    // Flipped turns it into the producer
    val inputIn = Flipped(Decoupled(Vec(layer.m, Vec(layer.n, UInt(8.W)))))
    val outputOut = Decoupled(Vec(layer.m, Vec(layer.k, UInt(8.W))))
  })

  require(layer.PEsPerOutput >= 1 && layer.PEsPerOutput <= layer.n)
  require(layer.n % layer.PEsPerOutput == 0)
  val cycles = layer.n / layer.PEsPerOutput

  // TODO: Consider storing this in BRAM?
  val weights = VecInit(layer.weights.map(row =>
    VecInit(row.map(w => w.U(8.W)))
  ))

  // State of the compution
  val cycleCounter = RegInit(0.U(log2Ceil(cycles + 1).W))
  val computing = RegInit(false.B)
  // TODO: Will this be stored in BRAM?
  val inputReg = Reg(Vec(layer.m, Vec(layer.n, UInt(8.W))))

  // One accumulator per output element (m*k total)
  val accumulators = Reg(Vec(layer.m, Vec(layer.k, UInt(32.W))))

  // Assumption: FIFOs always have space, so we don't need to wait for output to be consumed
  io.inputIn.ready := !computing

  val isComputing = io.inputIn.fire || computing

  // Start computing immeditely as the input gets data
  // We assume all the inputs are ready at this point
  when(io.inputIn.fire && !computing) {
    inputReg := io.inputIn.bits
    computing := true.B
    cycleCounter := 1.U
  }.elsewhen(computing && cycleCounter < (cycles - 1).U) {
    cycleCounter := cycleCounter + 1.U
  }.elsewhen(computing && cycleCounter === (cycles - 1).U) {
    // Computation done, immediately become ready for next input
    computing := false.B
    cycleCounter := 0.U
  }

  // Compute and accumulate
  for (i <- 0 until layer.m) {
    for (j <- 0 until layer.k) {
      // Compute partial sum using layer.PEsPerOutput multipliers
      var partialSum = 0.U
      for (pe <- 0 until layer.PEsPerOutput) {
        val idx = cycleCounter * layer.PEsPerOutput.U + pe.U
        // For the first cycle we use the input data, otherwise we use the regs
        val inputVal = Mux(io.inputIn.fire, io.inputIn.bits(i)(idx), inputReg(i)(idx))
        partialSum = partialSum + inputVal * weights(idx)(j)
      }

      // Accumulate
      when(isComputing) {
        when(io.inputIn.fire && !computing) {
          // First cycle
          accumulators(i)(j) := partialSum
        }.otherwise {
          accumulators(i)(j) := accumulators(i)(j) + partialSum
        }
      }
    }
  }

  // Output is valid one cycle after computation completes
  io.outputOut.valid := RegNext(isComputing && cycleCounter === (cycles - 1).U, false.B)
  io.outputOut.bits := accumulators
}