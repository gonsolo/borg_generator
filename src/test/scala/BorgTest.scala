import borg._
import chisel3._
import chisel3.simulator.EphemeralSimulator._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem.{Attachable, InterruptBusWrapper, PeripheryBus, PeripheryBusParams, PBUS}
import freechips.rocketchip.tilelink.TLFragmenter
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp, LazyRawModuleImp}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

import Constants._

import org.chipsalliance.cde.config.{Parameters, Field, Config}

import freechips.rocketchip.tilelink.TLBusWrapper

class SocIo extends Bundle {
  val x = Bool()
}

class SocModuleImp(_outer: Soc) extends LazyModuleImp(_outer) {

  val io = IO(new SocIo)
  io.x := true.B

  val parameters = new PeripheryBusParams(8, 8)
  val pbus = LazyModule(new PeripheryBus(parameters, "pbus"))
  val borg = pbus { LazyModule(new Borg(pbus.beatBytes)(p)) }
  pbus.coupleTo("borg-borg") { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
  printf(cf"SocModuleImp step\n")
}

class Soc()(implicit p: Parameters) extends LazyModule {
  val allClockGroupsNode = ClockGroupIdentityNode()
  val busContextName = "subsystem"
  lazy val module = Module(new SocModuleImp(this))
  printf(cf"Soc step\n")
}

class TestSoc extends Module {
  val parameters = new WithBorg()
  val soc = LazyModule(new Soc()(parameters))
  val io = IO(new SocIo)
  io.x := soc.module.io.x

  printf(cf"TestSoc step\n")
}

class SocTest extends AnyFlatSpec {
  behavior of "Soc"
  it should "instantiate" in {
    simulate(new TestSoc()) { soc =>
      soc.clock.step()
      soc.io.x.expect(true.B)
    }
  }
}

