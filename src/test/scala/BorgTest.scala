import borg._
import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters

class ResetTest extends AnyFlatSpec {
  behavior of "BorgLoader"
  it should "count correctly" in {
    simulate(new BorgLoader()) { loader =>
      println("Reset ok.")
        loader.clock.step()
        loader.io.counter.expect(0.U)

        println("Poke 1")
        loader.io.start_in.poke(1.U)
        loader.clock.step()
        println("Step")
        loader.io.counter.expect(0.U)
        println("counter: " + loader.io.counter.peek().litValue)
        println("start: " + loader.io.start_out.peek().litValue)

        loader.clock.step()
        println("Step")
        println("counter: " + loader.io.counter.peek().litValue)
        println("start: " + loader.io.start_out.peek().litValue)

        loader.clock.step()
        println("Step")
        println("counter: " + loader.io.counter.peek().litValue)
        println("start: " + loader.io.start_out.peek().litValue)

        loader.clock.step()
        println("Step")
        println("counter: " + loader.io.counter.peek().litValue)
        println("start: " + loader.io.start_out.peek().litValue)
    }
  }
}

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
