import borg._
import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

import Constants._

class TestModule extends Module {
  val instructionPort = IO(Flipped(new MemoryPortIo))
  instructionPort.request.ready := DontCare
  instructionPort.response.valid := DontCare
  instructionPort.response.bits.data := DontCare

  val dmaSize = 8 // bytes
  val instructionSize = dmaSize / 4 // 2 instructions
  val instructionWidth = 32

  val scratchPadMemory = Module(new AsyncScratchPadMemory(
    num_core_ports = 2,
    instructionSize = instructionSize,
    instructionWidth = instructionWidth))
  for (port <- scratchPadMemory.io.core_ports) {
    port.request.ready := DontCare
    port.request.valid := DontCare
    port.request.bits.function := DontCare
    port.request.bits.address := DontCare
    port.request.bits.data := DontCare
    port.response.valid := DontCare
    port.response.bits.data := DontCare
  }

  scratchPadMemory.io.core_ports(IPORT) <> instructionPort
}

class BorgTest extends AnyFlatSpec {

  behavior of "Borg"
  it should "instantiate" in {
    simulate(new TestModule) { test =>
        println("Reset ok.")
        test.clock.step()
        println("Step ok.")
    }
  }
}

class BorgMemoryTest extends AnyFlatSpec {

  val instruction1 = "b00000000000000000000010100010011".U // r0 = mov rZ
  val instruction2 = "b10101010101010101010101010101010".U // test data

  behavior of "Memory"
  it should "read and write" in {
    simulate(new TestModule) { test =>
      test.instructionPort.request.bits.address.poke(0)
      test.instructionPort.request.bits.function.poke(M_XWRITE)
      test.instructionPort.request.bits.data.poke(instruction1)
      test.instructionPort.request.valid.poke(true.B)
      test.clock.step()
      test.instructionPort.request.bits.address.poke(1)
      test.instructionPort.request.bits.function.poke(M_XWRITE)
      test.instructionPort.request.bits.data.poke(instruction2)
      test.instructionPort.request.valid.poke(true.B)
      test.clock.step()
      test.instructionPort.request.bits.address.poke(0)
      test.instructionPort.request.bits.function.poke(M_XREAD)
      test.instructionPort.request.valid.poke(true.B)
      test.clock.step()
      test.instructionPort.response.valid.expect(1)
      test.instructionPort.response.bits.data.expect(instruction1)
      test.clock.step()
      test.instructionPort.request.bits.address.poke(1)
      test.instructionPort.request.bits.function.poke(M_XREAD)
      test.instructionPort.request.valid.poke(true.B)
      test.clock.step()
      test.instructionPort.response.valid.expect(1)
      test.instructionPort.response.bits.data.expect(instruction2)
     }
  }

}

class BorgCoreTest extends AnyFlatSpec {

  class TestBorgCore extends Module {
    val parameters = new WithBorg()
    val core = Module(new BorgCore()(parameters))
    core.io := DontCare
  }

  behavior of "Core"
  it should "not fail" in {
    val parameters = new WithBorg()
    simulate(new TestBorgCore) { core =>
      core.clock.step()
      core.clock.step()
      core.clock.step()
      core.clock.step()
    }
  }
}

