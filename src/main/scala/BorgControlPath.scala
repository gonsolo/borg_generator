// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._

import Constants._
import Instructions._

class BorgControlPathIo() extends Bundle {
  val dat = Flipped(new DatToCtlIo())
  val ctl = new CtlToDatIo()
  val imem = Flipped(new MemoryPortIo())
  val dmem = Flipped(new MemoryPortIo())
}

class BorgControlPath() extends Module
{
  // Input and output signals for the control unit
  val io = IO(new BorgControlPathIo())
  io.imem.request := DontCare
  io.dmem := DontCare

  // Look up the incoming instruction and set the ALU operation accordingly
  val csignals = ListLookup(
    io.dat.instruction,
                       List(OP1_X,      ALU_X,        WB_X,       REN_0,  MEMORY_DISABLE,   MEMORY_X,     LOAD_0),
    Array(
      // instruction        op1 select  alu function  writeback   rf wen  memory            read/write    is_load
      LUI           -> List(OP1_IMU,    ALU_COPY1,    WB_ALU,     REN_1,  MEMORY_DISABLE,   MEMORY_X,     LOAD_0),
      LW            -> List(OP1_RS1,    ALU_ADD,      WB_MEM,     REN_1,  MEMORY_ENABLE,    MEMORY_READ,  LOAD_1),
      SW            -> List(OP1_RS1,    ALU_ADD,      WB_X,       REN_0,  MEMORY_ENABLE,    MEMORY_WRITE, LOAD_0),
      ADDI          -> List(OP1_RS1,    ALU_ADD,      WB_ALU,     REN_1,  MEMORY_DISABLE,   MEMORY_X,     LOAD_0)
    )
  )

  // Put the alu function into a variable
  val cs_operand1_select :: cs_alu_fun :: cs_wb_sel :: (cs_rf_wen: Bool) :: (cs_memory_enable: Bool) :: cs_memory_function :: (cs_is_load: Bool) :: Nil = csignals

  val waitingForMem = RegInit(false.B)
  val wbSel = RegInit(cs_wb_sel)
  val rfWen = RegInit(cs_rf_wen)
  val storedRd = RegInit(io.dat.instruction(RD_MSB, RD_LSB))
  val operand1Select = RegInit(cs_operand1_select)
  when (!waitingForMem) {
    wbSel := cs_wb_sel
    rfWen := cs_rf_wen
    storedRd := io.dat.instruction(RD_MSB, RD_LSB)
    operand1Select := cs_operand1_select
  }

  val stall = !io.imem.response.valid || !((cs_memory_enable && io.dmem.response.valid) || !cs_memory_enable) || waitingForMem
  printf(cf"  stall: $stall, instruction: ${io.dat}, waitingForMem: $waitingForMem\n")

  when (io.dmem.response.valid) {
    waitingForMem := false.B
  }

  // Set the data path control signals
  io.ctl.stall := stall
  io.ctl.alu_fun := cs_alu_fun
  io.ctl.operand1_select := operand1Select
  io.ctl.wb_sel := wbSel

  //io.ctl.rf_wen := Mux(stall, false.B, cs_rf_wen)
 
  io.ctl.rf_wen := rfWen
  io.ctl.stored_rd := storedRd

  printf(cf"  dmem request valid: $cs_memory_enable\n")
  io.dmem.request.valid := cs_memory_enable
  when (cs_is_load) {
    waitingForMem := true.B
  }
  io.dmem.request.bits.address := 0x5100.U
  //io.dmem.request.bits.function := cs_memory_function
  io.dmem.request.bits.function := MEMORY_READ
  io.dmem.request.bits.data := DontCare
  io.dmem.request.ready := DontCare
}

