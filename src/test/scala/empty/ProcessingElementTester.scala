package empty

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ProcessingElementTester extends AnyFlatSpec with ChiselScalatestTester {

  "ProcessingElementNew" should "correctly switch between buffers" in {
    test(new ProcessingElementNew).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.enable.poke(false.B)
      dut.io.acceptWeightIn.poke(false.B)
      dut.io.switchWeightIn.poke(false.B)
      dut.io.inputIn.poke(0.U)
      dut.io.partialSumIn.poke(0.U)
      dut.clock.step()

      // Load weight0 = 5 (currentWeight starts at 0, so weight1 gets loaded)
      dut.io.acceptWeightIn.poke(true.B)
      dut.io.weightIn.poke(5.U)
      dut.clock.step()
      dut.io.acceptWeightIn.poke(false.B)
      dut.clock.step()

      // Verify weight0 is active and weight1 = 5
      dut.io.weightOut.expect(0.U)

      // Perform computation with weight0 (should be 0)
      dut.io.enable.poke(true.B)
      dut.io.inputIn.poke(3.U)
      dut.io.partialSumIn.poke(0.U)
      dut.clock.step()
      dut.io.enable.poke(false.B)
      dut.clock.step()
      dut.io.partialSumOut.expect(0.U)

      // Switch to weight1
      dut.io.switchWeightIn.poke(true.B)
      dut.clock.step()
      dut.io.switchWeightIn.poke(false.B)
      dut.clock.step()

      // Verify weight1 is now active
      dut.io.weightOut.expect(5.U)

      // Load weight0 = 10 while using weight1
      dut.io.acceptWeightIn.poke(true.B)
      dut.io.weightIn.poke(10.U)
      dut.clock.step()
      dut.io.acceptWeightIn.poke(false.B)
      dut.clock.step()

      // Perform computation with weight1 = 5
      dut.io.enable.poke(true.B)
      dut.io.inputIn.poke(4.U)
      dut.io.partialSumIn.poke(100.U)
      dut.clock.step()
      dut.io.enable.poke(false.B)
      dut.clock.step()
      dut.io.partialSumOut.expect(120.U)

      // Switch to weight0
      dut.io.switchWeightIn.poke(true.B)
      dut.clock.step()
      dut.io.switchWeightIn.poke(false.B)
      dut.clock.step()

      // Verify weight0 is now active with value 10
      dut.io.weightOut.expect(10.U)

      // Perform computation with weight0 = 10
      dut.io.enable.poke(true.B)
      dut.io.inputIn.poke(2.U)
      dut.io.partialSumIn.poke(50.U)
      dut.clock.step()
      dut.io.enable.poke(false.B)
      dut.clock.step()
      dut.io.partialSumOut.expect(70.U)
      dut.io.weightOut.expect(10.U)
    }
  }
}
