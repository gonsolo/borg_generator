// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._

import ALU._
import Constants._
import Instructions._

class BorgControlPathIo() extends Bundle {
  val dat = Flipped(new DatToCtlIo())
  val ctl = new CtlToDatIo()
  //val imem = Flipped(new MemoryPortIo())
  val imem   = Flipped(new FrontEndCpuIo())
  val dmem = Flipped(new MemoryPortIo())
}

class BorgControlPath() extends Module
{
  val io = IO(new BorgControlPathIo())
  io := DontCare

  val csignals = ListLookup(io.imem.response.bits.inst,
    List(SODOR_N, OP1_X, OP2_X, ALU_X, WB_X, REN_0, SODOR_N, SODOR_MEN_0, MEMORY_X, MT_X, SODOR_CSR_N),
    Array(
      AUIPC -> List(SODOR_Y, OP1_IMU, OP2_PC , ALU_ADD , WB_ALU, REN_1, SODOR_Y, SODOR_MEN_0, MEMORY_X, MT_X, SODOR_CSR_N),
      ADDI  -> List(SODOR_Y, OP1_RS1, OP2_IMI, ALU_ADD , WB_ALU, REN_1, SODOR_Y, SODOR_MEN_0, MEMORY_X, MT_X, SODOR_CSR_N),
      FLW   -> List(SODOR_Y, OP1_RS1, OP2_IMI, ALU_ADD , WB_MEM, REN_1, SODOR_N, SODOR_MEN_1, MEMORY_READ, MT_W, SODOR_CSR_N),
      FADD_S-> List(SODOR_Y, OP1_RS1, OP2_RS2, ALU_ADD , WB_ALU, REN_1, SODOR_N, SODOR_MEN_0, MEMORY_X, MT_X, SODOR_CSR_N),
      FSW   -> List(SODOR_Y, OP1_RS1, OP2_IMS, ALU_ADD , WB_X  , REN_0, SODOR_N, SODOR_MEN_1, MEMORY_READ, MT_W, SODOR_CSR_N)
    )
  )

  val (cs_inst_val: Bool) :: cs_op1_sel :: cs_op2_sel :: cs_alu_fun :: cs_wb_sel :: (cs_rf_wen: Bool) :: (cs_bypassable: Bool) :: (cs_mem_en: Bool) :: cs_mem_fcn :: cs_msk_sel :: cs_csr_cmd :: Nil = csignals

  val ctrl_valid = io.imem.response.valid

  val take_evec = WireDefault(false.B)

  io.imem.request.valid := ctrl_valid

  io.ctl.exe_kill   := take_evec
  io.ctl.pc_sel     := Mux(take_evec, SODOR_PC_EXC, SODOR_PC_4)
  io.ctl.brjmp_sel  := false.B
  io.ctl.op1_sel    := cs_op1_sel
  io.ctl.op2_sel    := cs_op2_sel
  io.ctl.alu_fun    := cs_alu_fun
  io.ctl.wb_sel     := cs_wb_sel
  io.ctl.rf_wen     := Mux(!ctrl_valid, false.B, cs_rf_wen)
  io.ctl.bypassable := cs_bypassable
  io.ctl.csr_cmd    := Mux(!ctrl_valid, SODOR_CSR_N, cs_csr_cmd)
  io.ctl.dmem_val   := cs_mem_en && ctrl_valid && !take_evec
  io.ctl.dmem_function   := cs_mem_fcn
  io.ctl.dmem_typ   := cs_msk_sel
  io.ctl.exception  := !cs_inst_val && io.imem.response.valid
  take_evec         := RegNext(io.ctl.exception) || io.dat.sodor_csr_eret

//  val io = IO(new BorgControlPathIo())
//  io.imem.request := DontCare
//  io.dmem := DontCare
//  io.n_imem := DontCare
//  io.ctl := DontCare
//
//  // Look up the incoming instruction and set the ALU operation accordingly
//  val csignals = ListLookup(
//    io.dat.instruction,
//                       List(OP1_X,      ALU_X,        WB_X,       REN_0,  MEMORY_DISABLE,   MEMORY_X,     LOAD_0),
//    Array(
//      // instruction        op1 select  alu function  writeback   rf wen  memory            read/write    is_load
//      LUI           -> List(OP1_IMU,    ALU_COPY1,    WB_ALU,     REN_1,  MEMORY_DISABLE,   MEMORY_X,     LOAD_0),
//      LW            -> List(OP1_RS1,    ALU_ADD,      WB_MEM,     REN_1,  MEMORY_ENABLE,    MEMORY_READ,  LOAD_1),
//      SW            -> List(OP1_RS1,    ALU_ADD,      WB_X,       REN_0,  MEMORY_ENABLE,    MEMORY_WRITE, LOAD_0),
//      ADDI          -> List(OP1_RS1,    ALU_ADD,      WB_ALU,     REN_1,  MEMORY_DISABLE,   MEMORY_X,     LOAD_0)
//    )
//  )
//
//  // Put the alu function into a variable
//  val cs_operand1_select :: cs_alu_fun :: cs_wb_sel :: (cs_rf_wen: Bool) :: (cs_memory_enable: Bool) :: cs_memory_function :: (cs_is_load: Bool) :: Nil = csignals
//
//  val waitingForMem = RegInit(false.B)
//  val wbSel = RegInit(cs_wb_sel)
//  val rfWen = RegInit(cs_rf_wen)
//  val storedRd = RegInit(io.dat.instruction(RD_MSB, RD_LSB))
//  val operand1Select = RegInit(cs_operand1_select)
//  when (!waitingForMem) {
//    wbSel := cs_wb_sel
//    rfWen := cs_rf_wen
//    storedRd := io.dat.instruction(RD_MSB, RD_LSB)
//    operand1Select := cs_operand1_select
//  }
//
//  val stall = !io.imem.response.valid || !((cs_memory_enable && io.dmem.response.valid) || !cs_memory_enable) || waitingForMem
//  printf(cf"  stall: $stall, instruction: ${io.dat}, waitingForMem: $waitingForMem\n")
//
//  when (io.dmem.response.valid) {
//    waitingForMem := false.B
//  }
//
//  // Set the data path control signals
//  io.ctl.stall := stall
//  io.ctl.alu_fun := cs_alu_fun
//  io.ctl.operand1_select := operand1Select
//  io.ctl.wb_sel := wbSel
//
//  //io.ctl.rf_wen := Mux(stall, false.B, cs_rf_wen)
//
//  io.ctl.rf_wen := rfWen
//  io.ctl.stored_rd := storedRd
//
//  printf(cf"  dmem request valid: $cs_memory_enable\n")
//  io.dmem.request.valid := cs_memory_enable
//  when (cs_is_load) {
//    waitingForMem := true.B
//  }
//  io.dmem.request.bits.address := 0x5100.U
//  //io.dmem.request.bits.function := cs_memory_function
//  io.dmem.request.bits.function := MEMORY_READ
//  io.dmem.request.bits.data := DontCare
//  io.dmem.request.ready := DontCare
}

