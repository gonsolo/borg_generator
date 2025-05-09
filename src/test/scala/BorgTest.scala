// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import chisel3.util.{Enum, is, switch}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, TransferSizes}
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink.{
  TLBundle,
  TLClientNode,
  TLEdgeOut,
  TLFragmenter,
  TLManagerNode,
  TLManagerParameters,
  TLMasterParameters,
  TLMasterPortParameters,
  TLMessages,
  TLSlaveParameters,
  TLSlavePortParameters
}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.diplomacy.lazymodule.{
  LazyModule,
  LazyModuleImp
}

class BorgFirstDriver(edge: TLEdgeOut, address: BigInt) extends Module {
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

class BorgFirstHarness(implicit p: Parameters) extends LazyModule {
  val dut = LazyModule(new Borg(8))
  val driverNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "borgDriver")))
    )
  )

  dut.registerNode := TLFragmenter(8, 64) := driverNode

  lazy val module = Module(new Imp)
  class Imp extends LazyModuleImp(this) {
    val (out, edge) = driverNode.out(0)
    val driver = Module(new BorgFirstDriver(edge, 0x4000))

    out.a <> driver.io.tl.a
    out.d <> driver.io.tl.d

    val io = IO(new BorgIO)
    io.success := driver.io.success
  }
}

class BorgIO extends Bundle {
  val success = Output(Bool())
}

class BorgFirstTester(implicit p: Parameters) extends Module {
  val harness = LazyModule(new BorgFirstHarness())
  val io = IO(new BorgIO)
  io.success := harness.module.io.success
}

class BorgFirstTest extends AnyFlatSpec {
  behavior of "Borg"
  it should "do something" in {
    implicit val p: Parameters = Parameters.empty
    simulate(new BorgFirstTester()) { tester =>
      tester.clock.step(3)
      tester.io.success.expect(true.B)
    }
  }
}

class BorgKickDriver(edge: TLEdgeOut) extends Module {
  val io = IO(new Bundle {
    val tl = new TLBundle(edge.bundle)
    val success = Output(Bool())
  })

  val (s_write :: s_write_resp :: s_read :: s_read_resp :: s_done :: Nil) = Enum(5)
  val state = RegInit(s_write)

  val writeData = 1.U(32.W)
  val readData = 1.U(32.W)
  val kickAddress = 0x4020.U
  val completedAddress = 0x4040.U

  io.tl.a.valid := false.B
  io.tl.a.bits := DontCare
  io.tl.d.ready := true.B
  io.success := false.B

  val d_fired = RegNext(io.tl.d.fire)

  switch(state) {
    is(s_write) {
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Put(0.U, kickAddress, 2.U, writeData, 0xf.U)._2
      when(io.tl.a.fire) { state := s_read_resp }
    }
    is(s_write_resp) {
      when(d_fired && io.tl.d.bits.opcode === TLMessages.AccessAckData) {
        state := s_read
      }
    }
    is(s_read) {
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Get(0.U, completedAddress, 2.U)._2
      when(io.tl.a.fire) { state := s_read_resp }
    }
    is(s_read_resp) {
      when(d_fired && io.tl.d.bits.data =/= readData) {
        state := s_read
      }
      when(d_fired && io.tl.d.bits.data === readData) {
        state := s_done
      }
    }
    is(s_done) {
      io.success := true.B
    }
  }
}

class BorgKickHarness(implicit p: Parameters) extends LazyModule {
  val dut = LazyModule(new Borg(8))
  val driverNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "borgDriver")))
    )
  )

  dut.registerNode := TLFragmenter(8, 64) := driverNode

  //val driverNode2 = TLClientNode(
  //  Seq(
  //    TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "borgDriver2")))
  //  )
  //)
  //val driverNode2 = TLManagerNode(
  //  Seq(TLSlavePortParameters.v1(
  //    Seq(TLSlaveParameters.v1(
  //      Seq(AddressSet(0x4000, 0xf)))),
  //  8)))
  val driverNode2 = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0x5000, 0xFFF)),
      supportsGet = TransferSizes(1, 8),
      supportsPutFull = TransferSizes(1, 8),
      fifoId = Some(0)
    )),
    beatBytes = 8
  )))

  driverNode2 := TLFragmenter(8, 64) := dut.core.instructionCache.masterNode



  lazy val module = Module(new Imp)
  class Imp extends LazyModuleImp(this) {
    val (out, edge) = driverNode.out(0)
    val driver = Module(new BorgKickDriver(edge))

    out.a <> driver.io.tl.a
    out.d <> driver.io.tl.d

    val io = IO(new BorgIO)
    io.success := driver.io.success


    //val (out2, edge2) = driverNode2.out(0)
  }
}

class BorgKickTester(implicit p: Parameters) extends Module {
  val harness = LazyModule(new BorgKickHarness())
  val io = IO(new BorgIO)
  io.success := harness.module.io.success
}

class BorgKickTest extends AnyFlatSpec {
  behavior of "Borg"
  it should "do something" in {
    implicit val p: Parameters = Parameters.empty
    simulate(new BorgKickTester()) { tester =>
      tester.clock.step(13)
      tester.io.success.expect(true.B)
    }
  }
}
