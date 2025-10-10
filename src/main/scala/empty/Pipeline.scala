package empty

import chisel3._
import chisel3.util.{Decoupled, Queue}

class Pipeline(layers: Array[DenseLayer], fifoDepthBetweenLayers: Int = 2) extends Module {
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
    val inputIn = Flipped(Decoupled(Vec(firstLayer.m, Vec(firstLayer.n, UInt(8.W)))))
    val outputOut = Decoupled(Vec(lastLayer.m, Vec(lastLayer.k, UInt(8.W))))
  })

  val denseModules = layers.map(layer => Module(new DenseDataflowFold(layer)))

  denseModules.head.io.inputIn <> io.inputIn

  // Insert FIFOs between layers to decouple them:
  // - A layer can produce output and immediately start on new input
  // - The next layer consumes from FIFO when ready
  for (i <- 0 until denseModules.length - 1) {
    val fifo = Module(new Queue(
      Vec(layers(i).m, Vec(layers(i).k, UInt(8.W))),
      fifoDepthBetweenLayers
    ))
    fifo.io.enq <> denseModules(i).io.outputOut
    denseModules(i + 1).io.inputIn <> fifo.io.deq
  }

  // Connect last layer directly to output
  io.outputOut <> denseModules.last.io.outputOut
}
