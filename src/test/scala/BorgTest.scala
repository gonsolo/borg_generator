package borg

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.subsystem.{Attachable, InterruptBusWrapper, PeripheryBus, PeripheryBusParams, PBUS}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp, LazyRawModuleImp}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

import Constants._

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

