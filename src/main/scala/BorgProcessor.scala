// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import Constants._

class BorgCoreIo() extends Bundle
{
  val imem = new MemoryPortIo()
  val reset = Input(Bool())
  val startAddress = Input(UInt(32.W))
}
//
//case class BorgCoreParams(
//  xprlen: Int = 32
//) {}
//
//class DatToCtlIo() extends Bundle {
//  val inst   = Output(UInt(32.W))
//}
//
//trait ScalarOpConstants
//{
//  // ALU Operation Signal
//  val ALU_X   = 0.asUInt(4.W) // unspecified alu function
//  val ALU_ADD = 1.asUInt(4.W) // add alu function
//}
//
//object Constants extends ScalarOpConstants with RISCVConstants {}
//
//import Constants._
//
//class BorgControlPathIo() extends Bundle {
//  val dat = Flipped(new DatToCtlIo())
//  val ctl = new CtlToDatIo()
//}
//
//object Instructions
//{
//  def ADDI               = BitPat("b?????????????????000?????0010011")
//}
//
//import Instructions._
//
class BorgControlPath() extends Module
{
//  // Input and output signals for the control unit
//  val io = IO(new BorgControlPathIo())
//  io.ctl.alu_fun := DontCare
//  // Look up the incoming instruction and set the ALU operation accordingly
//  val csignals = ListLookup(
//    io.dat.inst,
//    List(              ALU_X),
//    Array(
//    // instruction   | alu function
//      ADDI    -> List( ALU_ADD)
//    )
//  )
//
//  // Put the alu function into a variable
//  val cs_alu_fun :: Nil = csignals
//
//  // Set the data path control signals
//  io.ctl.alu_fun := cs_alu_fun
}

//// Signals from the control unit to the data path unit
//class CtlToDatIo() extends Bundle() {
//
//  // The control unit decodes the instruction and set the correspong alu function for the data path unit
//  val alu_fun = Output(UInt(ALU_X.getWidth.W))
//}

class BorgDataPathIo() extends Bundle()
{
//  val ctl = Flipped(new CtlToDatIo())
//  val dat = new DatToCtlIo()
  val imem = Flipped(new MemoryPortIo())
  val reset = Input(Bool())
  val startAddress = Input(UInt(32.W))
}
//
//trait RISCVConstants {
//  val RD_MSB = 11
//  val RD_LSB = 7
//  val RS1_MSB = 19
//  val RS1_LSB = 15
//}
//
class BorgDataPath() extends Module
{
  val io = IO(new BorgDataPathIo())

  val programCounter = RegInit(0.U(32.W))
  programCounter := Mux(io.reset, io.startAddress, programCounter + 4.U)
  //printf(cf"Borg program counter: $programCounter\n")

  io.imem.request.bits.address := programCounter
  io.imem.request.bits.function := M_XREAD
  io.imem.request.bits.data := DontCare
  io.imem.request.valid := true.B
  printf(cf"BorgDataPath request valid: ${io.imem.request.valid}\n")
  io.imem.request.ready := DontCare

  val instruction = Mux(io.imem.response.valid, io.imem.response.bits.data, BUBBLE)
  printf(cf"Borg instruction: 0x$instruction%x\n")

//  val regfile = Mem(32, UInt(conf.xprlen.W))
//
//  val rs1_addr = inst(RS1_MSB, RS1_LSB)
//
//  val rs1_data = regfile(rs1_addr)
//
//  // immediates
//  val imm_i = inst(31, 20)
//
//  // sign-extend immediates
//  val imm_i_sext = Cat(Fill(20,imm_i(11)), imm_i)
//
//  // For now: ADDI is always register source 1
//  val alu_op1 = rs1_data
//
//  // For now: ADDI is always immediate
//  val alu_op2 = imm_i_sext
//
//  val alu_out = Wire(UInt(conf.xprlen.W))
//
//  alu_out := MuxCase(0.U, unsafeWrapArray(Array(
//      (io.ctl.alu_fun === ALU_ADD) -> (alu_op1 + alu_op2).asUInt
//    )))
//
//  val wb_data = Wire(UInt(conf.xprlen.W))
//
//  wb_data := alu_out
//
//  // Writeback write enable
//  val wb_wen = true.B // TODO
//
//  // The address to write back to
//  val wb_addr = inst(RD_MSB, RD_LSB)
//
//  when (wb_wen && (wb_addr =/= 0.U))
//  {
//    regfile(wb_addr) := wb_data
//  }
//
//  // To control unit
//  io.dat.inst := inst
}

class BorgCoreModule(outer: BorgCore) extends LazyModuleImp(outer)
{
  val io = IO(new BorgCoreIo())
  io := DontCare

  val c  = Module(new BorgControlPath())
  val d  = Module(new BorgDataPath())
  d.io.imem.response := DontCare
  d.io.imem.request.bits := DontCare
  d.io.imem.request.ready := DontCare
  d.io.imem.request.bits.address := DontCare
  d.io.imem.request.bits.data := DontCare
  d.io.imem.request.bits.function := DontCare
  d.io.reset := DontCare
  d.io.startAddress := DontCare

  val instructionCache = outer.instructionCache.module
  instructionCache.io.request <> d.io.imem.request
  instructionCache.io.response := DontCare
  instructionCache.io.request.ready := DontCare
  instructionCache.io.request.bits := DontCare

  //instructionCache.io := DontCare
  //d.io := DontCare

  //instructionCache.io.request := io.imem.request
//  io.imem.resp.valid := DontCare
//  io.imem.resp.bits := DontCare
//  io.debug_out := d.io.debug_out
//
//  d.io.imem.resp := DontCare
////
////  // Connect the control unit to the data path unit
////  // For example the control unit decodes an instruction and informs the data path unit
////  // about the alu function
////  c.io.ctl <> d.io.ctl
////  c.io.dat <> d.io.dat
//
//  io.imem <> d.io.imem
//  io.reset <> d.io.reset
//
////  d.io.reset_vector := io.reset_vector
}

class BorgCore()(implicit p: Parameters) extends LazyModule
{
  lazy val module = new BorgCoreModule(this)
  val instructionCache = LazyModule(new TrivialInstructionCache())
}

