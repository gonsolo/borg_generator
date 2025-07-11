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
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class BorgInstructionCacheModule(outer: BorgInstructionCache) extends LazyModuleImp(outer) {

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
      //printf(cf"Borg icache idle\n")
      when (io.request.valid) {
        state := s_request
      }
    }
    is (s_request) {
      assert(address >= "h5000".U)
      printf(cf"icache request 0x${address}%x\n")
      //mem.a.valid := true.B
      mem.a.bits := edge.Get(0.U, address, 2.U)._2
      //    mem.d.ready := false.B
      when (edge.done(mem.a)) {
        state := s_response
      }
    }
    is (s_response) {
      printf(cf"Borg icache response: ${mem.d.bits.data}\n")
      mem.d.ready := true.B
      when (mem.d.fire) {
        //printf(cf"  d fire\n")
        io.response.bits.data := mem.d.bits.data
        io.response.valid := true.B
      }
      when (edge.done(mem.d)) {
        //printf(cf"  edge done d\n")
        state := s_idle
      }
    }
  }
}

class BorgInstructionCache(implicit p: Parameters) extends LazyModule {
  lazy val module = new BorgInstructionCacheModule(this)

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
