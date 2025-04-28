import borg._
import chisel3._
import chisel3.simulator.EphemeralSimulator._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem.{Attachable, InterruptBusWrapper, PeripheryBus, PeripheryBusParams, PBUS}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp, LazyRawModuleImp}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

import Constants._

import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.resources.SimpleDevice

class SocIo extends Bundle {
  val x = Bool()
}

class SocModuleImp(_outer: Soc) extends LazyModuleImp(_outer) {

  val io = IO(new SocIo)
  io.x := true.B

  //val parameters = new PeripheryBusParams(8, 8)
  //val pbus = LazyModule(new PeripheryBus(parameters, "pbus"))

  //val node = pbus { TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
  //  name = "init-zero", sourceId = IdRange(0, 1)))))) }
  //pbus.coupleFrom("init-zero") { _ := node }

  //val regAddress: BigInt = 0x4000
  //val regSize: BigInt = 0x0FFF
  //val device = new SimpleDevice("borg-device", Seq("borg,borg-1"))
  //val registerNode = TLRegisterNode(Seq(AddressSet(regAddress, regSize)), device, "reg/control", beatBytes=pbus.beatBytes)
  //pbus.coupleTo("borg-borg") { registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }

  //val borg = LazyModule(new Borg(pbus.beatBytes)(p))
  //io.x := borg.module.io.x
  //pbus.coupleTo("borg-borg") { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }

  printf(cf"SocModuleImp step\n")
}

class Soc()(implicit p: Parameters) extends LazyModule {
  val allClockGroupsNode = ClockGroupIdentityNode()
  val busContextName = "subsystem"
  lazy val module = Module(new SocModuleImp(this))

  //val parameters = new PeripheryBusParams(8, 8)
  //val pbus = LazyModule(new PeripheryBus(parameters, "pbus"))
 
  //pbus.clockGroupNode := ClockGroupIdentityNode()
  //val regAddress: BigInt = 0x4000
  //val regSize: BigInt = 0x0FFF
  //val device = new SimpleDevice("borg-device", Seq("borg,borg-1"))
  //val registerNode = TLRegisterNode(Seq(AddressSet(regAddress, regSize)), device, "reg/control", beatBytes=pbus.beatBytes)
  //pbus.coupleTo("borg-borg") { registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }

  //val borg = LazyModule(new Borg(8)(p))

  printf(cf"Soc step\n")
}

//class TestSoc extends Module {
//  val parameters = new WithBorg()
//  val soc = LazyModule(new Soc()(parameters))
//  val io = IO(new SocIo)
//  io.x := soc.module.io.x
//
//  printf(cf"TestSoc step\n")
//}
//
//class SocTest extends AnyFlatSpec {
//  behavior of "Soc"
//  it should "instantiate" in {
//    simulate(new TestSoc()) { soc =>
//      soc.clock.step()
//      soc.io.x.expect(true.B)
//    }
//  }
//}

class TestSocTL(implicit p: Parameters)  extends Module {

  val io = IO(new Bundle {
    val start = Input(Bool())
    val result = Output(UInt(32.W))
    val done = Output(Bool())
  })

  val top = LazyModule(new TestTop())
  top.module.io.start := io.start
  io.result := top.module.io.result
  io.done := top.module.io.done
}

class SimpleTLDiplomaticSpec extends AnyFlatSpec {
  behavior of "SimpleTL"
  it should "do something" in {
    implicit val p: Parameters = Parameters.empty
    simulate(new TestSocTL()) { testSoc =>
      testSoc.io.start.poke(true)
      testSoc.clock.step()
      testSoc.io.start.poke(false)

      var cycles = 0
      testSoc.clock.step()
      while (!testSoc.io.done.peek().litToBoolean && cycles < 20) {
        testSoc.clock.step()
        cycles += 1
      }

      val result = testSoc.io.result.peek().litValue
      assert(result == (0x10 * 2), s"Unexpected result: got $result, expected ${0x10 * 2}")
      println(s"Test passed! Received result: $result")
    }

  }
}

