// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

// This test demonstrates how to drive a register via TileLink messages.
// This way registers can be tested without resorting to FPGA-accelerated 
// full system simulation with  FireSim.

package borg

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import chisel3.util.{Enum, is, switch}
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.tilelink.{
  TLBundle,
  TLClientNode,
  TLEdgeOut,
  TLFragmenter,
  TLMasterParameters,
  TLMasterPortParameters,
  TLMessages,
  TLRegisterNode
}
import org.chipsalliance.cde.config.{Parameters}
import org.chipsalliance.diplomacy.lazymodule.{
  LazyModule,
  LazyModuleImp
}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class Register(address: BigInt)(implicit p: Parameters) extends LazyModule {
  val node = TLRegisterNode(
    address = Seq(AddressSet(address, 0xfff)),
    device = new SimpleDevice("my-reg-device", Seq("myvendor,myreg"))
  )

  lazy val module = new Imp
  class Imp extends LazyModuleImp(this) {
    val register = RegInit(0.U(32.W))
    node.regmap(
      0x0 -> Seq(RegField(32, register))
    )
  }
}

class RegisterDriver(edge: TLEdgeOut, address: BigInt) extends Module {
  val io = IO(new Bundle {
    val tl = new TLBundle(edge.bundle)
    val success = Output(Bool())
  })

  val (s_idle :: s_write :: s_write_resp :: s_read :: s_read_resp :: s_done :: Nil) = Enum(6)
  val state = RegInit(s_idle)

  val writeData = 0xdeadbeefL.U(32.W)
  val addr = address.U

  io.tl.a.valid := false.B
  io.tl.a.bits := DontCare
  io.tl.d.ready := true.B
  io.success := false.B

  val d_fired = RegNext(io.tl.d.fire)

  switch(state) {
    is(s_idle) {
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Put(0.U, addr, 2.U, writeData, 0xf.U)._2
      when(io.tl.a.fire) {
        state := s_write_resp
      }
    }
    is(s_write_resp) {
      when(d_fired && io.tl.d.bits.opcode === TLMessages.AccessAckData) {
        state := s_read
      }
    }
    is(s_read) {
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Get(0.U, addr, 2.U)._2
      when(io.tl.a.fire) { state := s_read_resp }
    }
    is(s_read_resp) {
      when(d_fired) {
        when(io.tl.d.bits.data === writeData) {
          state := s_done
        }
      }
    }
    is(s_done) {
      io.success := true.B
    }
  }
}

class RegisterIO extends Bundle {
  val success = Output(Bool())
}

class RegisterHarness(implicit p: Parameters) extends LazyModule {
  val dut = LazyModule(new Register(0x1000))
  val driverNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "testDriver")))
    )
  )

  dut.node := TLFragmenter(4, 64) := driverNode

  lazy val module = Module(new Imp)
  class Imp extends LazyModuleImp(this) {
    val (out, edge) = driverNode.out(0)
    val driver = Module(new RegisterDriver(edge, 0x1000))

    out.a <> driver.io.tl.a
    out.d <> driver.io.tl.d

    val io = IO(new RegisterIO)
    io.success := driver.io.success
  }
}

class RegisterTester(implicit p: Parameters) extends Module {
  val harness = LazyModule(new RegisterHarness())
  val io = IO(new RegisterIO)
  io.success := harness.module.io.success
}

class RegisterTest extends AnyFlatSpec {
  behavior of "Register"
  it should "return the written value" in {
    implicit val p: Parameters = Parameters.empty
    simulate(new RegisterTester()) { tester =>
      tester.clock.step(5)
      tester.io.success.expect(true.B)
    }
  }
}
