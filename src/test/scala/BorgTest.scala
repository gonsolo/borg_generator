// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import chisel3.util.{Enum, is, switch}
import freechips.rocketchip.tilelink.{
  TLBundle,
  TLClientNode,
  TLEdgeOut,
  TLFragmenter,
  TLMasterParameters,
  TLMasterPortParameters,
  TLMessages
}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.diplomacy.lazymodule.{
  LazyModule,
  LazyModuleImp
}

class BorgDriver(edge: TLEdgeOut, address: BigInt) extends Module {
  val io = IO(new Bundle {
    val tl = new TLBundle(edge.bundle)
    val success = Output(Bool())
  })

  val (s_read :: s_read_resp :: s_done :: Nil) = Enum(3)
  val state = RegInit(s_read)

  val readData = 666.U(32.W)
  val addr = address.U

  io.tl.a.valid := false.B
  io.tl.a.bits := DontCare
  io.tl.d.ready := true.B
  io.success := false.B

  val d_fired = RegNext(io.tl.d.fire)

  switch(state) {
    is(s_read) {
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Get(0.U, addr, 2.U)._2
      when(io.tl.a.fire) { state := s_read_resp }
    }
    is(s_read_resp) {
      when(d_fired) {
        when(io.tl.d.bits.data === readData) {
          state := s_done
        }
      }
    }
    is(s_done) {
      io.success := true.B
    }
  }
}

class BorgHarness(implicit p: Parameters) extends LazyModule {
  val dut = LazyModule(new Borg(8))
  val driverNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "borgDriver")))
    )
  )

  dut.registerNode := TLFragmenter(4, 64) := driverNode

  lazy val module = Module(new Imp)
  class Imp extends LazyModuleImp(this) {
    val (out, edge) = driverNode.out(0)
    val driver = Module(new BorgDriver(edge, 0x4000))

    out.a <> driver.io.tl.a
    out.d <> driver.io.tl.d

    val io = IO(new RegisterIO)
    io.success := driver.io.success
  }
}

class BorgTester(implicit p: Parameters) extends Module {
  val harness = LazyModule(new RegisterHarness())
  val io = IO(new RegisterIO)
  io.success := harness.module.io.success
}

class BorgTest extends AnyFlatSpec {
  behavior of "Borg"
  it should "do something" in {
    implicit val p: Parameters = Parameters.empty
    simulate(new BorgTester()) { tester =>
      tester.clock.step(5)
      tester.io.success.expect(true.B)
    }
  }
}
