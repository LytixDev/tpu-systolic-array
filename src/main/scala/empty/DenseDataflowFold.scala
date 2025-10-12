package empty

import chisel3._
import chisel3.util.{log2Ceil, Decoupled, Queue}

/*
 * Dense layer with folding
 * NOTE: Assumes 8 bit quantization of inputs and weights with 32 bit accumulators
 * TODO: the above note
 *
 * numberOfPEs = number of multipliers per output channel/k
 * Total multipliers = m * k * numberOfPEs
 * Takes n / numberOfPEs cycles to compute
 *
 * Uses Decoupled (ready/valid) interfaces for flow control between layers.
 * Each layer has an internal output FIFO for buffering results, enabling true pipelining
 */
class DenseDataflowFold(layer: DenseLayer, outFifoDepth: Int = 2) extends Module {
  val io = IO(new Bundle{
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

  // Computation state
  val cycleCounter = RegInit(0.U(log2Ceil(cycles + 1).W))
  val computing = RegInit(false.B)
  // TODO: So we load from the FIFO and into the regs. Can we just load from the FIFO and avoid storing it in the regs?
  val inputReg = Reg(Vec(layer.m, Vec(layer.n, UInt(8.W)))) // TODO: Is this stored in BRAM?

  // One accumulator per output element (m*k total)
  // TODO: maybe with the FIFOs we can optimize this? i.e maybe we need less
  val accumulators = Reg(Vec(layer.m, Vec(layer.k, UInt(32.W))))

  // NOTE: Incurs a 1 cycle latency by default w/o the flow = true param
  // TODO: So the FIFOs give us decoupling between layers. However, if two layers are perfectly in sync, they don't
  //       necessarily need to be decoupled and perhaps we could directly wire them together?
  val outputFifo = Module(new Queue(Vec(layer.m, Vec(layer.k, UInt(8.W))), outFifoDepth, flow=true))

  // Can accept input when not computing
  // output FIFOs handles buffering
  io.inputIn.ready := !computing

  // Start computing immeditely as the input gets data
  val isComputing = io.inputIn.fire || computing

  when(io.inputIn.fire && !computing) {
    inputReg := io.inputIn.bits
    computing := true.B
    cycleCounter := 1.U
  }.elsewhen(computing && cycleCounter < (cycles - 1).U) {
    cycleCounter := cycleCounter + 1.U
  }.elsewhen(computing && cycleCounter === (cycles - 1).U) {
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

  // Connect computation results to output FIFO
  // TODO: Are there scenarios where the downstream layer can start eagerly working on partial results?
  outputFifo.io.enq.valid := RegNext(isComputing && cycleCounter === (cycles - 1).U, false.B)
  outputFifo.io.enq.bits := accumulators

  // External output comes from the output FIFO
  io.outputOut <> outputFifo.io.deq
}