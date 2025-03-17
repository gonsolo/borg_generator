import borg._
import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class BorgTest extends AnyFlatSpec {

  class TestModule extends Module {
    // Use the same memory as in the actual hardware.
    // This should be factored out later.
    val dmaSize = 16 * 64 // 1024 bytes
    val instructionSize = dmaSize / 4 // 256 instructions, 32 bits/4 bytes wide
    val instructionWidth = UInt(32.W)
    val memory = Mem(instructionSize, instructionWidth) // 256 instructions a 4 bytes = 1024 bytes
    memory(0) := "b00000000000000000000010100010011".U // r0 = mov rZ
    memory(1) := "b10101010101010101010101010101010".U // test data

    //val scratchPadMemory = Module(new AsyncScratchPadMemory(num_core_ports = 2, memory = memory))
    // TODO
    //scratchPadMemory.io.core_ports(1).resp.ready := DontCare
    //scratchPadMemory.io.core_ports(1).resp.valid := DontCare
    //scratchPadMemory.io.core_ports(1).resp.bits := DontCare

    val params = new WithBorg()
    //val core = Module(new BorgCore()(params))
    //core.io.imem <> scratchPadMemory.io.core_ports(0)
  }

  behavior of "Borg"
  it should "instantiate" in {
    simulate(new TestModule) { test =>
        println("Reset ok.")
        test.clock.step()
        println("Step ok.")
    }
  }
}

