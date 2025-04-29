package borg

import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class Harness(implicit p: Parameters) extends LazyModule {
  val dut = LazyModule(new Register(0x1000))
  val driverNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "testDriver")))))

  dut.node := TLFragmenter(4, 64) := driverNode

  lazy val module = Module(new Imp)
  class Imp extends LazyModuleImp(this) {
    val (out, edge) = driverNode.out(0)
    val driver = Module(new Driver(edge, 0x1000))

    out.a <> driver.io.tl.a
    out.d <> driver.io.tl.d

    val io = IO(new Bundle {
      val success = Output(Bool())
    })
    io.success := driver.io.success
  }
}

