import borg._
import chisel3._
import chisel3.simulator.EphemeralSimulator._
import freechips.rocketchip.prci.ClockGroupIdentityNode
import freechips.rocketchip.subsystem.{Attachable, InterruptBusWrapper, PeripheryBus, PeripheryBusParams, PBUS}
import freechips.rocketchip.tilelink.TLFragmenter
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp, LazyRawModuleImp}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

import Constants._

class SocModuleImp(_outer: Soc) extends LazyModuleImp(_outer) {
  val parameters = new PeripheryBusParams(8, 8)
  val pbus = new PeripheryBus(parameters, "pbus")
  val borg = pbus { LazyModule(new Borg(pbus.beatBytes)(p)) }
  pbus.coupleTo("borg-borg") { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
  printf(cf"SocModuleImp step\n")
}

class Soc()(implicit p: Parameters) extends LazyModule with Attachable {
  val allClockGroupsNode = ClockGroupIdentityNode()
  val ibus = LazyModule(new InterruptBusWrapper)
  val busContextName = "subsystem"
  lazy val module = new SocModuleImp(this)
  printf(cf"Soc step\n")
}

class TestSoc extends Module {
  val parameters = new WithBorg()
  val soc = LazyModule(new Soc()(parameters))
  printf(cf"TestSoc step\n")
}

class SocTest extends AnyFlatSpec {

  behavior of "Borg"
  it should "instantiate" in {
    simulate(new TestSoc) { test =>
        println("Reset ok.")
        test.clock.step()
        test.clock.step()
        test.clock.step()
        test.clock.step()
        println("Steps ok.")
    }
  }
}

