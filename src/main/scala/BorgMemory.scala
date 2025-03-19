// Copyright Andreas Wendleder 2025
// GPL-3.0-only

package borg

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

trait MemoryOpConstants {
  val DPORT = 0
  val IPORT = 1

  val M_X = "b0".asUInt(1.W)
  val M_XREAD = "b0".asUInt(1.W)
  val M_XWRITE= "b1".asUInt(1.W)
}

object Constants extends MemoryOpConstants {}

import Constants._

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

  when (io.core_ports(IPORT).request.valid) {
    printf(cf"Borg scratchpad request valid\n")
    io.core_ports(IPORT).response.valid := RegNext(io.core_ports(IPORT).request.valid)
    switch (io.core_ports(IPORT).request.bits.function) {
      is (M_XREAD) {
        printf(cf"Borg scratchpad read\n")
        io.core_ports(IPORT).response.bits.data := memory(io.core_ports(IPORT).request.bits.address)
      }
      is (M_XWRITE) {
        printf(cf"Borg scratchpad write\n")
        memory(io.core_ports(IPORT).request.bits.address) := io.core_ports(IPORT).request.bits.data
      }
    }
  }
}

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

