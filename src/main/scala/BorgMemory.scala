// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange}
import freechips.rocketchip.tilelink.{
  TLClientNode,
  TLMasterParameters,
  TLMasterPortParameters
}
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import scala.language.reflectiveCalls

trait RISCVConstants {
  // The Bubble Instruction (Machine generated NOP)
  val BUBBLE = 0x4033.U(32.W)
}

trait MemoryOpConstants {
  val DPORT = 0
  val IPORT = 1

  val M_X = "b0".asUInt(1.W)
  val M_XREAD = "b0".asUInt(1.W)
  val M_XWRITE = "b1".asUInt(1.W)
}

object Constants extends RISCVConstants with MemoryOpConstants {}

import Constants._

class MemoryRequest() extends Bundle {
  val address = Output(UInt(32.W))
  val function = Output(UInt(M_X.getWidth.W))
  val data = Output(UInt(32.W))
}

class MemoryResponse() extends Bundle {
  val data = Output(UInt(32.W))
}

class MemoryPortIo() extends Bundle {
  val request = Flipped(new DecoupledIO(new MemoryRequest()))
  val response = new ValidIO(new MemoryResponse())
}

class TrivialInstructionCacheModule(outer: TrivialInstructionCache)
    extends LazyModuleImp(outer) {
  val s_idle :: s_request :: s_response :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // TileLink port to memory.
  val (mem, edge) = outer.node.out(0)

  // IO between Core and ICache.
  val io = IO(new MemoryPortIo)
  io.request.ready := state === s_idle

  val address = RegNext(io.request.bits.address)

  mem.a.valid := state === s_request
  io.response.valid := false.B
  io.response.bits.data := 0.U

  switch (state) {
    is (s_idle) {
      printf(cf"Borg icache idle\n")
      when (io.request.valid) {
        state := s_request
      }
    }
    is (s_request) {
      printf(cf"Borg icache request 0x${address}%x\n")
      //mem.a.valid := true.B
      mem.a.bits := edge.Get(0.U, address, 2.U)._2
      //    mem.d.ready := false.B
      when (edge.done(mem.a)) {
        state := s_response
      }
    }
    is (s_response) {
      printf(cf"Borg icache response\n")
      mem.d.ready := true.B
      when (mem.d.fire) {
        printf(cf"  d fire, data: ${mem.d.bits.data}%b\n")
        io.response.bits.data := mem.d.bits.data
        io.response.valid := true.B
      }
    }
  }
}

class TrivialInstructionCache(implicit p: Parameters) extends LazyModule {
  lazy val module = new TrivialInstructionCacheModule(this)

  // Connection to main memory.
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        Seq(
          TLMasterParameters.v1(
            name = "Borg Instruction Cache",
            sourceId = IdRange(0, 1)
          )
        )
      )
    )
  )
}
