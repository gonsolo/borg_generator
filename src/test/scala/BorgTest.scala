import borg._
import chisel3._
import chisel3.simulator.EphemeralSimulator._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockGroupIdentityNode, ClockSinkParameters}
import freechips.rocketchip.subsystem.{Attachable, InterruptBusWrapper, PeripheryBus, PeripheryBusParams, PBUS}
import freechips.rocketchip.tilelink.TLFragmenter
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp, LazyRawModuleImp}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

import Constants._

import org.chipsalliance.cde.config.{Parameters, Field, Config}

//class BlaIo extends Bundle {
//  val bla = Output(Bool())
//}

class SocModuleImp(_outer: Soc) extends LazyModuleImp(_outer) {
  //val io = IO(new BlaIo)
  //io.bla := true.B
  val parameters = new PeripheryBusParams(8, 8)
  val pbus = LazyModule(new PeripheryBus(parameters, "pbus"))
  val borg = pbus { LazyModule(new Borg(pbus.beatBytes)(p)) }
  pbus.coupleTo("borg-borg") { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
  printf(cf"SocModuleImp step\n")
}

//trait HasBlaIo {
//  def io: BlaIo
//}

class Soc()(implicit p: Parameters) extends LazyModule with Attachable {
  val allClockGroupsNode = ClockGroupIdentityNode()
  val ibus = LazyModule(new InterruptBusWrapper)
  val busContextName = "subsystem"
  override lazy val module = new SocModuleImp(this)

  //val bla = IO(Output(Bool())).suggestName("bla")
  //bla := DontCare

  printf(cf"Soc step\n")
}

class TestSoc extends Module {
  val parameters = new WithBorg()
  val soc = LazyModule(new Soc()(parameters))

  printf(cf"TestSoc step\n")
}

class MyBundle extends Bundle {
  val x = Bool()
}

class MyLazyModuleImp(outer: MyLazyModule) extends LazyModuleImp(outer) {
  val io = IO(new MyBundle())
  io.x := true.B
}

class MyLazyModule()(implicit p: Parameters) extends LazyModule {
  lazy val module = Module(new MyLazyModuleImp(this))
}

case object MyKey extends Field[Option[MyConfig]](None)
case class My()

class MyConfig() extends Config((site, here, up) => {
  case MyKey => Some(My())
})

class MyModule extends Module {
  val parameters = new MyConfig()
  val myLazyModule = LazyModule(new MyLazyModule()(parameters))
  val io = IO(new MyBundle())
  io.x := myLazyModule.module.io.x
}

class MyTest extends AnyFlatSpec {

  behavior of "My"
  it should "instantiate" in {
    simulate(new MyModule()) { m =>
      m.io.x.expect(true.B)
    }
  }
}

