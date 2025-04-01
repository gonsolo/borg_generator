// Copyright Andreas Wendleder 2025
// GPL-3.0-only

package borg

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange}
import freechips.rocketchip.tilelink.{TLClientNode, TLMasterParameters, TLMasterPortParameters}
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import scala.language.reflectiveCalls

trait RISCVConstants {
  // The Bubble Instruction (Machine generated NOP)
  val BUBBLE  = 0x4033.U(32.W)
}

trait MemoryOpConstants {
  val DPORT = 0
  val IPORT = 1

  val M_X = "b0".asUInt(1.W)
  val M_XREAD = "b0".asUInt(1.W)
  val M_XWRITE= "b1".asUInt(1.W)
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
  val request    = new DecoupledIO(new MemoryRequest())
  val response   = Flipped(new ValidIO(new MemoryResponse()))
}

class AsyncScratchPadMemory(num_core_ports: Int, instructionSize: Int, instructionWidth: Int) extends Module
{
  val io = IO(new Bundle {
    val core_ports = Vec(num_core_ports, Flipped(new MemoryPortIo()))
  })

  for (port <- io.core_ports) {
    port.request.ready := DontCare
    port.request.valid := DontCare
    port.request.bits.function := DontCare
    port.request.bits.data := DontCare
    port.response.valid := DontCare
    port.response.bits.data := DontCare
  }

  val memory = Mem(instructionSize, UInt(instructionWidth.W))

  //for ( i <- 0 to 1) { printf(cf"Borg memory $i: 0b${memory(i)}%b\n") }

  when (io.core_ports(IPORT).request.valid) {
    io.core_ports(IPORT).response.valid := RegNext(io.core_ports(IPORT).request.valid)
    switch (io.core_ports(IPORT).request.bits.function) {
      is (M_XREAD) {
        io.core_ports(IPORT).response.bits.data := memory(io.core_ports(IPORT).request.bits.address)
      }
      is (M_XWRITE) {
        memory(io.core_ports(IPORT).request.bits.address) := io.core_ports(IPORT).request.bits.data
      }
    }
  }
}

class TrivialInstructionCacheRequest extends Bundle
{
  val address = UInt(32.W)
}

class TrivialInstructionCacheResponse extends Bundle
{
  val data = UInt(32.W)
}

class TrivialInstructionCacheBundle extends Bundle
{
  val request = Flipped(Decoupled(new TrivialInstructionCacheRequest))
  val response = Valid(new TrivialInstructionCacheResponse)
}

class TrivialInstructionCacheModule(outer: TrivialInstructionCache) extends LazyModuleImp(outer)
{
  // TileLink port to memory.
  val (mem, edge) = outer.masterNode.out(0)

  // IO between Core and ICache.
  val io = IO(new TrivialInstructionCacheBundle)

  val s_idle :: s_request :: s_response :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val addressBits = edge.bundle.addressBits
  val address = Reg(UInt(addressBits.W))

  switch (state) {
    is (s_idle) {
      mem.a.valid := false.B
      mem.d.ready := false.B
      io.request.ready := true.B
      when (io.request.valid === true.B) {
        state := s_request
        address := io.request.bits.address
      }
    }
    is (s_request) {
      mem.a.valid := true.B
      mem.d.ready := false.B
      when (edge.done(mem.a)) {
        state := s_response
      }
    }
    is (s_response) {
      mem.a.valid := false.B
      mem.d.ready := true.B
      when (mem.d.valid === true.B) {
        io.response.bits.data := mem.d.bits.data
        io.response.valid := true.B
      }
      when (mem.d.valid === false.B) {
        state := s_idle
      }
    }
  }
}

class TrivialInstructionCache(implicit p: Parameters) extends LazyModule
{
  lazy val module = new TrivialInstructionCacheModule(this)

  // Connection to main memory.
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "Borg Instruction Cache",
    sourceId = IdRange(0, 1))))))
}
