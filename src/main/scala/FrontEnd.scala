package borg

import chisel3._
import chisel3.util._


import Constants._

class FrontEndRequest() extends Bundle {
  val pc   = UInt(32.W)
}

class FrontEndResponse() extends Bundle {
  val pc   = UInt(32.W)
  val inst = UInt(32.W)
}

class FrontEndCpuIo extends Bundle
{
  val request = Flipped(new ValidIO(new FrontEndRequest()))
  val response = new DecoupledIO(new FrontEndResponse())
}

class FrontEndIo extends Bundle
{
   val cpu  = new FrontEndCpuIo
   val imem = Flipped(new MemoryPortIo)
}

object DummyFrontEnd {
  val START_ADDR = "h5000".U

  val M_XRD = 0.U
  val MT_WU = 0.U
}

import DummyFrontEnd._

class FrontEnd extends Module
{
  val io = IO(new FrontEndIo)
  io := DontCare

  val if_reg_valid  = RegInit(false.B)
  val if_reg_pc     = RegInit(START_ADDR - 4.U)
  //printf(cf"  if_reg_pc: 0x$if_reg_pc%x\n")

  val exe_reg_valid = RegInit(false.B)
  val exe_reg_pc    = Reg(UInt(32.W))
  val exe_reg_inst  = Reg(UInt(32.W))

  //**********************************
  // Next PC Stage (if we can call it that)
  val if_pc_next = Wire(UInt(32.W))
  val if_val_next = WireInit(true.B) 

  val if_pc_plus4 = (if_reg_pc + 4.asUInt(32.W))

  if_val_next := io.imem.request.ready

  // stall IF/EXE if backend not ready
  when (io.cpu.response.ready && if_val_next)
  {
     //printf(cf"  ready: setting if_pc_next plus4: 0x$if_pc_plus4%x\n")
     if_pc_next := if_pc_plus4
     //when (io.cpu.request.valid)
     //{
     //   // datapath is redirecting the PC stream (misspeculation)
     //   if_pc_next := io.cpu.request.bits.pc
     //    printf(cf"  redirecting if_pc_next: 0x$if_pc_next%x\n")
     //}
  }
  .otherwise
  {
     //printf(cf"  otherwise: setting if_pc_next if_reg_pc: 0x$if_reg_pc%x\n")
     if_pc_next  := if_reg_pc
  }

  when (io.cpu.response.ready)
  {
     if_reg_pc    := if_pc_next
     //printf(cf"  setting if_reg_pc to if_pc_next: 0x$if_pc_next%x\n")
     if_reg_valid := if_val_next
  }


  // set up outputs to the instruction memory
  io.imem.request.bits.address   := if_pc_next
  io.imem.request.valid          := if_val_next
  io.imem.request.bits.function  := M_XRD
  io.imem.request.bits.typ       := MT_WU
  when (if_val_next) { 
    printf(cf"Frontend imem request address: 0x$if_pc_next%x\n")
  }

  //**********************************
  // Inst Fetch/Return Stage
  //printf(cf"Frontend: response ready: ${io.cpu.response.ready}\n")
  when (io.cpu.response.ready)
  {
     exe_reg_valid := if_reg_valid && !io.cpu.request.valid
     exe_reg_pc    := if_reg_pc
     exe_reg_inst  := io.imem.response.bits.data
     //printf(cf"Frontend: instruction: ${io.imem.response.bits.data}\n")
  }

  //**********************************
  // Execute Stage
  // (pass the instruction to the backend)
  io.cpu.response.valid     := exe_reg_valid
  io.cpu.response.bits.inst := exe_reg_inst
  io.cpu.response.bits.pc   := exe_reg_pc
}

