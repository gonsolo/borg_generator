// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._

import scala.collection.immutable.ArraySeq.unsafeWrapArray

import Constants._

class BorgDataPathIo() extends Bundle()
{
  val ctl = Flipped(new CtlToDatIo())
  val dat = new DatToCtlIo()
  val imem = Flipped(new MemoryPortIo())
  val dmem = Flipped(new MemoryPortIo())
  val startAddress = Input(UInt(32.W))
}

class BorgDataPath() extends Module
{
  val io = IO(new BorgDataPathIo())
  io.dmem := DontCare
  io.imem.request.bits.typ := DontCare

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

  printf(cf"  operand1_select: 0x${io.ctl.operand1_select}%x\n")
  printf(cf"  rs1_data: 0x${rs1_data}%x\n")
  printf(cf"  imm_i_sext: 0x${imm_i_sext}%x\n")
  printf(cf"  imm_u_sext: 0x${imm_u_sext}%x\n")
  printf(cf"  alu_op1: 0x${alu_op1}%x\n")
  printf(cf"  alu_op2: 0x${alu_op2}%x\n")
  printf(cf"  alu_out: 0x${alu_out}%x\n")

  val wb_data = Wire(UInt(64.W))

  wb_data := MuxCase(alu_out, unsafeWrapArray(Array(
    (io.ctl.wb_sel === WB_ALU) -> alu_out,
    (io.ctl.wb_sel === WB_MEM) -> io.dmem.response.bits.data
  )))


  // Writeback write enable
  val wb_wen = io.ctl.rf_wen

  // The address to write back to
  //val wb_addr = instruction(RD_MSB, RD_LSB)
  val wb_addr = io.ctl.stored_rd

  printf(cf"  dmem response data: 0x${io.dmem.response.bits.data}%x\n")
  printf(cf"  wb_sel: 0x${io.ctl.wb_sel}%x\n")
  printf(cf"  wb_data: 0x${wb_data}%x\n")
  printf(cf"  wb_wen: 0x${wb_wen}%x\n")
  printf(cf"  wb_addr: 0x${wb_addr}%x\n")

  when (wb_wen && (wb_addr =/= 0.U))
  {
    regfile(wb_addr) := wb_data
  }

  printf(cf"Register  5: 0x${regfile(5)}%x\n")
  printf(cf"Register 10: 0x${regfile(10)}%x\n")

  val address_written = RegNext(wb_addr)

  // To control unit
  io.dat.instruction := instruction

  // To data cache
  val alu_out_1 = RegNext(alu_out)
  val alu_out_2 = RegNext(alu_out_1)
  val alu_out_3 = RegNext(alu_out_2)
  printf(cf"  dmem request address: 0x$alu_out_3%x\n")
  io.dmem.request.bits.address := 0x5100.U //alu_out_3
  //io.dmem.request.bits.data := rs2_data.asUInt()
}

// Minimal RISC-V datapath supporting only: auipc, addi, flw, fadd.s, fsw

object ALU {
  val SZ_ALU_FN = 4
}

import ALU._

class AluIo extends Bundle {
  val fn = Input(UInt(SZ_ALU_FN.W))
  val in2 = Input(UInt(32.W))
  val in1 = Input(UInt(32.W))
  val out = Output(UInt(32.W))
}

class ALU extends Module {
   val io = IO(new AluIo)
}

class FrontEndCpuResponse() extends Bundle {
  val data = Output(UInt(32.W))
}

class FrontEndRequest() extends Bundle {
  val pc   = UInt(32.W)
}

class FrontEndResponse() extends Bundle {
  val pc   = UInt(32.W)
  val inst = UInt(32.W)
}

class FrontEndCpuIO extends Bundle
{
  val request = Flipped(new ValidIO(new FrontEndRequest()))
  val response = new DecoupledIO(new FrontEndResponse())
}

class CtrlSignals extends Bundle {
  val rf_wen        = Output(Bool())
  val op1_sel       = Output(UInt(2.W))
  val op2_sel       = Output(UInt(2.W))
  val alu_fun       = Output(UInt(SZ_ALU_FN.W))
  val dmem_val      = Output(Bool())
  val dmem_function = Output(UInt(MEMORY_X.getWidth.W))
  val dmem_typ      = Output(UInt(3.W))
  val wb_sel        = Output(UInt(2.W))
}

class NewBorgDataPathIo extends Bundle {
  val imem = Flipped(new FrontEndCpuIO())
  val dmem = new MemoryPortIo()
  val ctl = Input(new CtrlSignals())
  val dat = new DatToCtlIo()
}

class NewBorgDataPath extends Module {
  val io = IO(new NewBorgDataPathIo())
  io := DontCare

  val wb_reg_valid = RegInit(false.B)
  val wb_reg_ctrl = Reg(new CtrlSignals)
  val wb_reg_pc = Reg(UInt(32.W))
  val wb_reg_alu = Reg(UInt(32.W))
  val wb_reg_wbaddr = Reg(UInt(log2Ceil(32).W))

  val exe_valid = io.imem.response.valid
  val exe_inst = io.imem.response.bits.inst
  val exe_pc = io.imem.response.bits.pc

  val exe_rs1_addr = exe_inst(RS1_MSB, RS1_LSB)
  val exe_rs2_addr = exe_inst(RS2_MSB, RS2_LSB)
  val exe_wbaddr = exe_inst(RD_MSB, RD_LSB)

  val wb_wbdata = Wire(UInt(32.W))

  val regfile = Mem(32, UInt(32.W))

  when(wb_reg_ctrl.rf_wen && (wb_reg_wbaddr =/= 0.U)) {
    regfile(wb_reg_wbaddr) := wb_wbdata
  }

  val rf_rs1_data = Mux(exe_rs1_addr =/= 0.U, regfile(exe_rs1_addr), 0.U)
  val rf_rs2_data = Mux(exe_rs2_addr =/= 0.U, regfile(exe_rs2_addr), 0.U)

  val imm_i = exe_inst(31, 20)
  val imm_s = Cat(exe_inst(31, 25), exe_inst(11, 7))
  val imm_u = Cat(exe_inst(31, 12), Fill(12, 0.U))

  val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i)
  val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)

  val exe_alu_op1 = Mux(io.ctl.op1_sel === OP1_IMU, imm_u, rf_rs1_data)
  val exe_alu_op2 = Mux(io.ctl.op2_sel === OP2_IMI, imm_i_sext,
                    Mux(io.ctl.op2_sel === OP2_IMS, imm_s_sext, rf_rs2_data))

  val alu = Module(new ALU())
  alu.io.in1 := exe_alu_op1
  alu.io.in2 := exe_alu_op2
  alu.io.fn := io.ctl.alu_fun

  val exe_alu_out = alu.io.out

  io.dmem.request.valid := io.ctl.dmem_val && exe_valid
  io.dmem.request.bits.function := io.ctl.dmem_function
  io.dmem.request.bits.typ := io.ctl.dmem_typ
  io.dmem.request.bits.address := exe_alu_out
  io.dmem.request.bits.data := rf_rs2_data

  wb_reg_valid := exe_valid
  wb_reg_ctrl := io.ctl
  wb_reg_pc := exe_pc
  wb_reg_alu := exe_alu_out
  wb_reg_wbaddr := exe_wbaddr

  wb_wbdata := MuxCase(wb_reg_alu, Seq(
    (wb_reg_ctrl.wb_sel === WB_ALU) -> wb_reg_alu,
    (wb_reg_ctrl.wb_sel === WB_MEM) -> io.dmem.response.bits.data
  ))
}

