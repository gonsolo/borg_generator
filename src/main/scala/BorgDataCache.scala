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

class BorgDataCacheModule(outer: BorgDataCache) extends LazyModuleImp(outer) {

  val s_idle :: s_request :: s_response :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // TileLink port to memory.
  val (mem, edge) = outer.node.out(0)

  // IO between Core and ICache.
  val io = IO(new MemoryPortIo)
  //io := DontCare
  io.request.ready := state === s_idle

  val address = RegNext(io.request.bits.address)
  //printf(cf" saving address: 0x${io.request.bits.address}%x\n")

  mem.a.valid := state === s_request
  io.response.valid := false.B
  io.response.bits.data := 0.U

  switch (state) {
    is (s_idle) {
      when (io.request.valid) {
        state := s_request
      }
    }
    is (s_request) {
      printf(cf" data cache a request address: 0x$address%x\n")
      mem.a.bits := edge.Get(2.U, address, 2.U)._2
      when (edge.done(mem.a)) {
        state := s_response
      }
    }
    is (s_response) {
      mem.d.ready := true.B
      when (mem.d.fire) {
        printf(cf"  data cache d response data: 0x${mem.d.bits.data}%x\n")
        io.response.bits.data := mem.d.bits.data
        io.response.valid := true.B
      }
      when (edge.done(mem.d)) {
        state := s_idle
      }
    }
  }
}

class BorgDataCache(implicit p: Parameters) extends LazyModule {
  lazy val module = new BorgDataCacheModule(this)

  // Connection to main memory.
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        Seq(
          TLMasterParameters.v1(
            name = "Borg Data Cache",
            sourceId = IdRange(2, 3)
          )
        )
      )
    )
  )
}

