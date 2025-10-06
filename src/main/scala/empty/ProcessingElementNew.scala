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
    val switchWeightOut = Output(Bool())
    val inputOut = Output(UInt(8.W))
    val partialSumOut = Output(UInt(32.W))
    val enableOut = Output(Bool())
  })

  /*
   * There is an edge case where the first time we're loading weights into the SA we are both loading and later reading
   * from the same weight reg.
   *
   * This means that , we must load into the same reg as we later will be reading from.
                this is only true for the first time since every other execution will load weights into the reg that
                is not being read from.
   *
   * I tried using the same switching system but the problem
   */
  // val isInitial = RegInit(1.U(1.W))

  /*
   * After a switch weight signal we want to switch weights in the same cycle
   *
   * NOTE: This feels inelegant? What will this be synthesized into?
   */
  val currentWeightReg = RegInit(0.U(1.W))
  val currentWeight = Mux(io.switchWeightIn, ~currentWeightReg, currentWeightReg)
  // Update the register for the next cycle
  currentWeightReg := currentWeight

  val weight0 = RegInit(0.U(8.W))
  val weight1 = RegInit(0.U(8.W))
  val inputReg = RegInit(0.U(8.W))
  val partialSumReg = RegInit(0.U(32.W))

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

  io.weightOut := Mux(currentWeight === 0.U, weight1, weight0)
  io.switchWeightOut := RegNext(io.switchWeightIn, false.B)
  io.inputOut := inputReg
  io.partialSumOut := partialSumReg
  io.enableOut := RegNext(io.enable, false.B)
}