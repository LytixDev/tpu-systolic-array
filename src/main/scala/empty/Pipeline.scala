package empty

import chisel3._

class Pipeline(layers: Array[DenseLayer]) extends Module {
  require(layers.nonEmpty, "Pipeline must have at least one layer")

  // Validate that layers are compatible (output of layer i matches input of layer i+1)
  // NOTE: This should probably be done by the frontend
  for (i <- 0 until layers.length - 1) {
    require(layers(i).k == layers(i + 1).n,
      s"Layer $i output dimension (k=${layers(i).k}) must match layer ${i+1} input dimension (n=${layers(i+1).n})")
  }

  val firstLayer = layers.head
  val lastLayer = layers.last

  val io = IO(new Bundle {
    val inputIn = Input(Vec(firstLayer.m, Vec(firstLayer.n, UInt(8.W))))
    val inputValid = Input(Bool())
    val outputOut = Output(Vec(lastLayer.m, Vec(lastLayer.k, UInt(8.W))))
    val outputValid = Output(Bool())
  })

  val denseModules = layers.map(layer => Module(new DenseDataflowFold(layer)))

  // Connect first layer input
  denseModules.head.io.inputIn := io.inputIn
  denseModules.head.io.inputValid := io.inputValid

  // Chain layers together with valid signals
  for (i <- 0 until denseModules.length - 1) {
    denseModules(i + 1).io.inputIn := denseModules(i).io.outputOut
    denseModules(i + 1).io.inputValid := denseModules(i).io.outputValid
  }

  // Connect last layer output
  io.outputOut := denseModules.last.io.outputOut
  io.outputValid := denseModules.last.io.outputValid
}
