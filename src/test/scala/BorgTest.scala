import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import borg._
import org.chipsalliance.cde.config.Parameters

class BorgSpec extends AnyFlatSpec {
  behavior of "Borg"
  it should "be fully connected" in {
    val params = new WithBorg
    val conf = new BorgCoreParams
    simulate(new BorgCore()(params, conf)) { c =>
      println("Ok.")
    }
  }
}
