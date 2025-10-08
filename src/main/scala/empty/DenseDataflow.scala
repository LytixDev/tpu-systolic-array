package empty

import chisel3._

/*
 * input matrix: m*n
 * weight matrix: n*k
 * PEsPerOutput: number of multipliers per output (1 = fully folded, n = fully parallel)
 */
case class DenseLayer(m: Int, n: Int, k: Int, weights: Array[Array[Int]], PEsPerOutput: Int) {
  require(weights.length == n, s"weights must have $n rows, got ${weights.length}")
  require(weights.forall(_.length == k), s"all weight rows must have $k columns")
  require(1 <= PEsPerOutput && PEsPerOutput <= n, s"PEsPerOutput must be between 1 (fully folded) and $n (fully parallel)")
  require(n % PEsPerOutput == 0, s"n=$n must be divisible by PEsPerOutput=$PEsPerOutput")
}


/*
 * Implements a static weight-stationary streaming, fully parallel, dense layer
 * NOTE: rn we assume 8 bit quantization
 */
class DenseDataflow(layer: DenseLayer) extends Module {
  val io = IO(new Bundle{
    val inputIn = Input(Vec(layer.m, Vec(layer.n, UInt(8.W))))
    val outputOut = Output(Vec(layer.m, Vec(layer.k, UInt(8.W))))
  })

  /*
   * Convert weights to chisel literals (i.e hardwired constants).
   */
  val weights = VecInit(layer.weights.map(row =>
    VecInit(row.map(w => w.U(8.W)))
  ))

  // output[i][j] = sum over l of (input[i][l] * weights[l][j])
  for (i <- 0 until layer.m) { // for each row in the input
    for (j <- 0 until layer.k) { // for each col in the output
      // multiply input row i with weight column j
      var sum = 0.U
      // This could be optimized into a tree reduce if we want
      for (l <- 0 until layer.n) {
        sum = sum + io.inputIn(i)(l) * weights(l)(j)
      }
      io.outputOut(i)(j) := sum
    }
  }
}