package empty

import chisel3._

class ProcessingElementNew extends Module {
  val io = IO(new Bundle{
    val weightIn = Input(UInt(8.W))
    val acceptWeightIn = Input(Bool())
    val switchWeightIn = Input(Bool())
    val inputIn = Input(UInt(8.W))
    val partialSumIn = Input(UInt(32.W))

    // Control signals
    val enable = Input(Bool())

    val weightOut = Output(UInt(8.W))
    val inputOut = Output(UInt(8.W))
    val partialSumOut = Output(UInt(32.W))
    val enableOut = Output(Bool())
  })

  val currentWeight = RegInit(0.U(1.W))
  val weight0 = RegInit(0.U(8.W))
  val weight1 = RegInit(0.U(8.W))
  val inputReg = RegInit(0.U(8.W))
  val partialSumReg = RegInit(0.U(32.W))

  when (io.switchWeightIn) {
    currentWeight := ~currentWeight
  }

  when(io.acceptWeightIn) {
    // Accept the weight into the not currently selected weight
    when (currentWeight === 0.U) {
      weight1 := io.weightIn
    } .otherwise {
      weight0 := io.weightIn
    }
  }

  when(io.enable) {
    inputReg := io.inputIn
    partialSumReg := io.inputIn * Mux(currentWeight === 0.U, weight0, weight1) + io.partialSumIn
  }

  io.weightOut := Mux(currentWeight === 0.U, weight0, weight1)
  io.inputOut := inputReg
  io.partialSumOut := partialSumReg
  io.enableOut := RegNext(io.enable, false.B)
}