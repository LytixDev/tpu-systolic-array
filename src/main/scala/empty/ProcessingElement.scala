package empty

import chisel3._

class ProcessingElement extends Module {
  val io = IO(new Bundle{
    val weightIn = Input(UInt(8.W))
    val acceptWeightIn = Input(Bool())
    val inputIn = Input(UInt(8.W))
    val partialSumIn = Input(UInt(32.W))

    // Control signals
    val enable = Input(Bool())

    val weightOut = Output(UInt(8.W))
    val inputOut = Output(UInt(8.W))
    val partialSumOut = Output(UInt(32.W))
    val enableOut = Output(Bool())
  })

  val weightReg = RegInit(0.U(8.W))
  val inputReg = RegInit(0.U(8.W))
  val partialSumReg = RegInit(0.U(32.W))

  when(io.acceptWeightIn) {
    weightReg := io.weightIn
  }

  when(io.enable) {
    inputReg := io.inputIn
    partialSumReg := io.inputIn * weightReg + io.partialSumIn
  }

  io.weightOut := weightReg
  io.inputOut := inputReg
  io.partialSumOut := partialSumReg
  io.enableOut := RegNext(io.enable, false.B)
}