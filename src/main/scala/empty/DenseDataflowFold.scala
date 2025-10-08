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
class DenseDataflowFold(layer: DenseLayer, numberOfPEs: Int) extends Module {
  val io = IO(new Bundle{
    val inputIn = Input(Vec(layer.m, Vec(layer.n, UInt(8.W))))
    val outputOut = Output(Vec(layer.m, Vec(layer.k, UInt(32.W))))
  })

  require(numberOfPEs >= 1 && numberOfPEs <= layer.n)
  require(layer.n % numberOfPEs == 0)
  val cycles = layer.n / numberOfPEs

  val weights = VecInit(layer.weights.map(row =>
    VecInit(row.map(w => w.U(8.W)))
  ))

  // Counts from 0 to cycles-1
  val cycleCounter = RegInit(0.U(log2Ceil(cycles + 1).W))
  when(cycleCounter < cycles.U) {
    cycleCounter := cycleCounter + 1.U
  }

  // One accumulator per output element (m*k total)
  // TODO: There is some design space exploration available here
  val accumulators = Reg(Vec(layer.m, Vec(layer.k, UInt(32.W))))

  for (i <- 0 until layer.m) { // for each row in the input
    for (j <- 0 until layer.k) { // for each output element
      // Reset accumulator
      when(cycleCounter === 0.U) {
        accumulators(i)(j) := 0.U
      }

      // Compute partial sum using numberOfPEs multipliers
      // NOTE: this could be optimized using an adder tree or something
      var partialSum = 0.U
      for (pe <- 0 until numberOfPEs) {
        val idx = cycleCounter * numberOfPEs.U + pe.U
        partialSum = partialSum + io.inputIn(i)(idx) * weights(idx)(j)
      }

      // Accumulate the partial sum
      when(cycleCounter < cycles.U) {
        accumulators(i)(j) := accumulators(i)(j) + partialSum
      }

      // TODO: bake in quantization here
      io.outputOut(i)(j) := accumulators(i)(j)
    }
  }
}