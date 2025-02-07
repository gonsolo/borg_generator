import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import borg._
import org.chipsalliance.cde.config.Parameters

///class ResetTest extends AnyFlatSpec {
//  behavior of "BorgCore"
//  it should "be fully connected" in {
//    val params = new WithBorg
//    val conf = new BorgCoreParams
//    simulate(new BorgCore()(params, conf)) { core =>
//      println("Reset ok.")
//    }
//  }
//}

//class StepTest extends AnyFlatSpec {
//  behavior of "BorgCore"
//  it should "reset and step" in {
//    val params = new WithBorg
//    val conf = new BorgCoreParams
//    simulate(new BorgCore()(params, conf)) { core =>
//      core.io.reset_vector.poke(0.U)
//      core.clock.step()
//      core.io.debug_out.expect(4.U)
//      core.clock.step()
//      core.io.debug_out.expect(8.U)
//      core.clock.step()
//      core.io.debug_out.expect(12.U)
//      core.clock.step()
//      core.io.debug_out.expect(16.U)
//      core.reset.poke(1)
//      core.clock.step()
//      core.reset.poke(0)
//      core.clock.step()
//      core.io.debug_out.expect(4.U)
//      core.clock.step()
//      core.io.debug_out.expect(8.U)
//      println("Pc counter now: " + core.io.debug_out.peek().litValue)
//    }
//  }
//}
//
//class TileTest extends AnyFlatSpec {
//  behavior of "BorgTile"
//  it should "initialize" in {
//    val params = new WithBorg
//    val conf = new BorgCoreParams
//    simulate(new BorgTile()(params, conf)) { tile =>
//      // TODO
//    }
//  }
//}
