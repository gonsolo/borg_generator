// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import scala.collection.immutable.ArraySeq.unsafeWrapArray

import Constants._

class BorgCoreIo() extends Bundle
{
  val imem = new MemoryPortIo()
  val startAddress = Input(UInt(32.W))
}

class DatToCtlIo() extends Bundle {
  val instruction   = Output(UInt(32.W))
}

class BorgControlPathIo() extends Bundle {
  val dat = Flipped(new DatToCtlIo())
  val ctl = new CtlToDatIo()
  val imem = Flipped(new MemoryPortIo())
  val dmem = Flipped(new MemoryPortIo())
}

object Instructions
{
  def LUI  = BitPat("b?????????????????????????0110111")
  def LW   = BitPat("b?????????????????010?????0000011")
  def SW   = BitPat("b?????????????????010?????0100011")
  def ADDI = BitPat("b?????????????????000?????0010011")
}

import Instructions._

class BorgControlPath() extends Module
{
  // Input and output signals for the control unit
  val io = IO(new BorgControlPathIo())
  io.imem.request := DontCare
  io.dmem := DontCare

  // Look up the incoming instruction and set the ALU operation accordingly
  val csignals = ListLookup(
    io.dat.instruction,
                       List(OP1_X,      ALU_X,        WB_X,       MEMORY_UNDECIDED, MEMORY_X),
    Array(
      // instruction        op1 select  alu function  writeback   memory            read/write
      LUI           -> List(OP1_IMU,    ALU_COPY1,    WB_ALU,     MEMORY_DISABLE,   MEMORY_X),
      LW            -> List(OP1_RS1,    ALU_ADD,      WB_MEM,     MEMORY_ENABLE,    MEMORY_READ),
      SW            -> List(OP1_RS1,    ALU_ADD,      WB_X,       MEMORY_ENABLE,    MEMORY_WRITE),
      ADDI          -> List(OP1_RS1,    ALU_ADD,      WB_ALU,     MEMORY_DISABLE,   MEMORY_X)
    )
  )

  // Put the alu function into a variable
  val cs_operand1_select :: cs_alu_fun :: cs_wb_sel :: cs_memory_enable :: cs_memory_function :: Nil = csignals

  // Set the data path control signals
  io.ctl.alu_fun := cs_alu_fun
  io.ctl.operand1_select := cs_operand1_select

  val stall = !io.imem.response.valid || !( !(cs_memory_enable === MEMORY_ENABLE) || ((cs_memory_enable === MEMORY_ENABLE) && io.dmem.response.valid))
  io.ctl.stall := stall

  io.dmem.request.valid := cs_memory_enable
  io.dmem.request.bits.function := cs_memory_function
}

// Signals from the control unit to the data path unit
class CtlToDatIo() extends Bundle() {

  // The CPU is stalled when waiting for the instruction cache. The program counter is not updated then.
  val stall = Output(Bool())

  // The control unit decodes the instruction and set the corresponding alu function for the data path unit.
  val alu_fun = Output(UInt(ALU_X.getWidth.W))

  val operand1_select = Output(UInt(OP1_X.getWidth.W))
}

class BorgDataPathIo() extends Bundle()
{
  val ctl = Flipped(new CtlToDatIo())
  val dat = new DatToCtlIo()
  val imem = Flipped(new MemoryPortIo())
  val startAddress = Input(UInt(32.W))
}

class BorgDataPath() extends Module
{
  val io = IO(new BorgDataPathIo())

  val programCounter = RegInit(io.startAddress)
  val programCounterNext = RegInit(io.startAddress)
  programCounterNext := Mux(reset.asBool, io.startAddress, programCounter + 4.U)

  when (!io.ctl.stall) {
    programCounter := programCounterNext
  }

  io.imem.request.bits.address := programCounter
  io.imem.request.bits.function := MEMORY_READ
  io.imem.request.bits.data := DontCare
  io.imem.request.valid := Mux(reset.asBool, false.B, true.B)
  io.imem.request.ready := DontCare

  val instruction = Mux(io.imem.response.valid, io.imem.response.bits.data, BUBBLE)

  val regfile = Mem(32, UInt(64.W))

  // Decode
  val rs1_addr = instruction(RS1_MSB, RS1_LSB)

  val rs1_data = regfile(rs1_addr)

  // immediates
  val imm_i = instruction(31, 20)
  val imm_u = instruction(31, 12)

  // sign-extend immediates
  val imm_i_sext = Cat(Fill(20,imm_i(11)), imm_i)
  val imm_u_sext = Cat(imm_u, Fill(12, 0.U))

  // For now: ADDI is always register source 1
  val alu_op1 = MuxCase(0.U, unsafeWrapArray(Array(
    (io.ctl.operand1_select === OP1_RS1) -> rs1_data,
    (io.ctl.operand1_select === OP1_IMU) -> imm_u_sext
  ))).asUInt

  // For now: ADDI is always immediate
  val alu_op2 = imm_i_sext

  val alu_out = Wire(UInt(64.W))


  alu_out := MuxCase(0.U, unsafeWrapArray(Array(
      (io.ctl.alu_fun === ALU_ADD) -> (alu_op1 + alu_op2).asUInt,
      (io.ctl.alu_fun === ALU_COPY1) -> alu_op1
    )))

  val wb_data = Wire(UInt(64.W))

  wb_data := alu_out

  // Writeback write enable
  val wb_wen = true.B // TODO

  // The address to write back to
  val wb_addr = instruction(RD_MSB, RD_LSB)

  when (wb_wen && (wb_addr =/= 0.U))
  {
    regfile(wb_addr) := wb_data
  }

  printf(cf"Register 10: 0x${regfile(10)}%x\n")

  val address_written = RegNext(wb_addr)

  // To control unit
  io.dat.instruction := instruction
}

class BorgCoreModule(outer: BorgCore) extends LazyModuleImp(outer)
{
  val io = IO(new BorgCoreIo())
  io := DontCare

  val dataCache = outer.dataCache.module

  val c  = Module(new BorgControlPath())
  c.io.imem.request := DontCare
  c.io.dmem <> dataCache.io

  val d  = Module(new BorgDataPath())
  d.reset := reset
  d.io.startAddress := io.startAddress
  d.io.ctl <> c.io.ctl
  d.io.dat <> c.io.dat

  val instructionCache = outer.instructionCache.module
  instructionCache.reset := reset
  instructionCache.io.request <> d.io.imem.request
  d.io.imem.response <> instructionCache.io.response
  c.io.imem.response <> instructionCache.io.response

  // Connect the control unit to the data path unit
  // For example the control unit decodes an instruction and informs the data path unit
  // about the alu function
  // c.io.ctl <> d.io.ctl
//  c.io.dat <> d.io.dat

//  io.imem <> d.io.imem
//  io.reset <> d.io.reset
//
//  d.io.reset_vector := io.reset_vector
}

class BorgCore()(implicit p: Parameters) extends LazyModule
{
  lazy val module = new BorgCoreModule(this)
  val instructionCache = LazyModule(new TrivialInstructionCache())
  val dataCache = LazyModule(new TrivialDataCache())
}

