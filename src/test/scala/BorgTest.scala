import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import borg._
import org.chipsalliance.cde.config.Parameters

class BorgSpec extends AnyFlatSpec {
  behavior of "BorgCore"

  it should "be fully connected" in {
    val params = new WithBorg
    val conf = new BorgCoreParams
    simulate(new BorgCore()(params, conf)) { core =>
      println("Reset ok.")
    }
  }

  it should "reset and step" in {
    val params = new WithBorg
    val conf = new BorgCoreParams
    simulate(new BorgCore()(params, conf)) { core =>
      core.io.reset_vector.poke(0.U)
      core.clock.step()
      core.io.debug_out.expect(4.U)
      core.clock.step()
      core.io.debug_out.expect(8.U)
      core.clock.step()
      core.io.debug_out.expect(12.U)
      core.clock.step()
      core.io.debug_out.expect(16.U)
      println("Last pc counter: " + core.io.debug_out.peek().litValue)
    }
  }
}
