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

class Tester(implicit p: Parameters) extends Module {
  val harness = LazyModule(new Harness())

  val io = IO(new Bundle {
    val success = Output(Bool())
  })
  io.success := harness.module.io.success
 }

class RegisterSpec extends AnyFlatSpec {
  behavior `of` "Register"
  it `should` "read and write" in {
    implicit val p: Parameters = Parameters.empty
    simulate(new Tester()) { tester =>
      tester.clock.step(5)
      val success = tester.io.success.peek().litValue
      println(s"Received success: $success")
    }
  }
}

