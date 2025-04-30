package borg

import chisel3._
import chisel3.util.{Enum, is, switch}
import chisel3.simulator.EphemeralSimulator.simulate
import chisel3.simulator.PeekPokeAPI.{
  testableBool,
  testableClock,
  testableData,
  testableUInt
}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, TransferSizes}
import freechips.rocketchip.tilelink.{
  TLClientNode,
  TLManagerNode,
  TLMasterParameters,
  TLMasterPortParameters,
  TLSlaveParameters,
  TLSlavePortParameters
}
import org.chipsalliance.cde.config.{Parameters}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class Requester(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name = "SimpleRequester",
            sourceId = IdRange(0, 1)
          )
        )
      )
    )
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val start = Input(Bool())
      val result = Output(UInt(32.W))
      val done = Output(Bool())
    })

    val (out, edge) = node.out(0)

    val (sIdle :: sSend :: sWait :: sDone :: Nil) = Enum(4)
    val state = RegInit(sIdle)

    val sourceId = 0.U
    val addr = 0x10.U(32.W)

    out.a.valid := (state === sSend)
    out.a.bits := edge
      .Get(
        fromSource = sourceId,
        toAddress = addr,
        lgSize = 2.U // 4 bytes
      )
      ._2

    out.d.ready := (state === sWait)

    val resultReg = RegInit(0.U(32.W))
    io.result := resultReg
    io.done := (state === sDone)

    switch(state) {
      is(sIdle) {
        when(io.start) { state := sSend }
      }
      is(sSend) {
        when(out.a.ready) { state := sWait }
      }
      is(sWait) {
        when(out.d.valid) {
          resultReg := out.d.bits.data
          state := sDone
        }
      }
      is(sDone) {}
    }
  }
}

class Responder(implicit p: Parameters) extends LazyModule {

  val node = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address = Seq(AddressSet(0x0, 0xfff)), // 4KB memory
            supportsGet = TransferSizes(1, 4),
            supportsPutFull = TransferSizes(1, 4)
          )
        ),
        beatBytes = 4
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val (in, edge) = node.in(0)

    in.a.ready := true.B
    in.d.valid := RegNext(in.a.valid)

    // In this case we just respond with the sent address doubled
    val dataResp = in.a.bits.address * 2.U
    in.d.bits := edge.AccessAck(in.a.bits, dataResp)
  }
}

class TileLinkHarness(implicit p: Parameters) extends LazyModule {
  val requester = LazyModule(new Requester)
  val responder = LazyModule(new Responder)

  responder.node := requester.node // diplomatic connection

  lazy val module = Module(new Impl)
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val start = Input(Bool())
      val result = Output(UInt(32.W))
      val done = Output(Bool())
    })

    requester.module.io.start := io.start
    io.result := requester.module.io.result
    io.done := requester.module.io.done
  }
}

class TileLinkTester(implicit p: Parameters) extends Module {

  val io = IO(new Bundle {
    val start = Input(Bool())
    val result = Output(UInt(32.W))
    val done = Output(Bool())
  })

  val top = LazyModule(new TileLinkHarness())
  top.module.io.start := io.start
  io.result := top.module.io.result
  io.done := top.module.io.done
}

class TileLinkTest extends AnyFlatSpec {

  behavior of "TileLink"
  it should "respond with a double value" in {
    implicit val p: Parameters = Parameters.empty
    simulate(new TileLinkTester()) { testSoc =>
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
      assert(
        result == (0x10 * 2),
        s"Unexpected result: got $result, expected ${0x10 * 2}"
      )
      println(s"Test passed! Received result: $result")
    }
  }
}
