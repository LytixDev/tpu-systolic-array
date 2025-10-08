package empty

import chisel3._
import chisel3.util.log2Ceil

/*
 * Dense layer with folding
 * NOTE: Assumes 8 bit quantization of inputs and weights with 32 bit accumulators
 *
 * numberOfPEs = number of multipliers per output channel (per k)
 * Total multipliers = m * k * numberOfPEs
 * Takes n / numberOfPEs cycles to compute
 */
class DenseDataflowFold(layer: DenseLayer) extends Module {
  val io = IO(new Bundle{
    val inputIn = Input(Vec(layer.m, Vec(layer.n, UInt(8.W))))
    val inputValid = Input(Bool())
    val outputOut = Output(Vec(layer.m, Vec(layer.k, UInt(8.W))))
    val outputValid = Output(Bool())
  })

  require(layer.PEsPerOutput >= 1 && layer.PEsPerOutput <= layer.n)
  require(layer.n % layer.PEsPerOutput == 0)
  val cycles = layer.n / layer.PEsPerOutput

  val weights = VecInit(layer.weights.map(row =>
    VecInit(row.map(w => w.U(8.W)))
  ))

  // Counts from 0 to cycles-1
  val cycleCounter = RegInit(0.U(log2Ceil(cycles + 1).W))
  val computing = RegInit(false.B)

  // Immidetly start computing when the valid signal is given
  val isComputing = io.inputValid || computing
  when(io.inputValid && !computing) {
    computing := true.B
    cycleCounter := 1.U
  } .elsewhen(computing && cycleCounter < (cycles - 1).U) {
    cycleCounter := cycleCounter + 1.U
  } .elsewhen(computing && cycleCounter === (cycles - 1).U) {
    computing := false.B
    cycleCounter := 0.U
  }

  // One accumulator per output element (m*k total)
  // TODO: There is some design space exploration available here
  val accumulators = Reg(Vec(layer.m, Vec(layer.k, UInt(32.W))))

  for (i <- 0 until layer.m) { // for each row in the input
    for (j <- 0 until layer.k) { // for each output element
      // Compute partial sum using layer.PEsPerOutput multipliers
      // NOTE: this could be optimized using an adder tree or something
      var partialSum = 0.U
      for (pe <- 0 until layer.PEsPerOutput) {
        val idx = cycleCounter * layer.PEsPerOutput.U + pe.U
        partialSum = partialSum + io.inputIn(i)(idx) * weights(idx)(j)
      }

      // Accumulate
      when(isComputing) {
        when(io.inputValid && !computing) {
          // Starting fresh - just use the partial sum
          accumulators(i)(j) := partialSum
        }.otherwise {
          // Continue accumulating
          accumulators(i)(j) := accumulators(i)(j) + partialSum
        }
      }

      // TODO: bake in quantization here
      io.outputOut(i)(j) := accumulators(i)(j)
    }
  }

  io.outputValid := RegNext(isComputing && cycleCounter === (cycles - 1).U, false.B)
}