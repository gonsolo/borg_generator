// Copyright Andreas Wendleder 2025
// GPL-3.0-only

package borg

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters}
import scala.language.reflectiveCalls

trait MemoryOpConstants {
  val DPORT = 0
  val IPORT = 1
}

object Constants extends MemoryOpConstants {}

import Constants._

class AsyncScratchPadMemory(num_core_ports: Int, instructionSize: Int, instructionWidth: UInt) extends Module
{
  val io = IO(new Bundle {
    val core_ports = Vec(num_core_ports, Flipped(new MemPortIo()))
  })

  for (port <- io.core_ports) {
    port.req.ready := DontCare
    port.resp.valid := DontCare
    port.resp.bits.data := DontCare
  }

  val memory = Mem(instructionSize, instructionWidth)

  when (io.core_ports(IPORT).req.valid) {
    io.core_ports(IPORT).resp.valid := RegNext(io.core_ports(IPORT).req.valid)
    io.core_ports(IPORT).resp.bits.data := memory(io.core_ports(IPORT).req.bits.addr)
  }
}

class MemReq() extends Bundle {
  val addr = Output(UInt(32.W))
}

class MemResp() extends Bundle {
  val data = Output(UInt(32.W))
}

class MemPortIo() extends Bundle {
  val req    = new DecoupledIO(new MemReq())
  val resp   = Flipped(new ValidIO(new MemResp()))
}

class BorgCoreIo() extends Bundle
{
  val imem = new MemPortIo()
//  val reset_vector = Input(UInt(32.W))
//
//  val debug_out = Output(UInt(32.W))
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
//class MemReq(data_width: Int) extends Bundle
//{
//  val addr = Output(UInt(32.W))
//}
//
//class MemResp(data_width: Int) extends Bundle
//{
//  val data = Output(UInt(data_width.W))
//}
//
//class MemPortIo(data_width: Int) extends Bundle
//{
//  val req = new DecoupledIO(new MemReq(data_width))
//  val resp = Flipped(new ValidIO(new MemResp(data_width)))
//}
//
//class BorgDpathIo(implicit val conf: BorgCoreParams) extends Bundle()
//{
//  val ctl = Flipped(new CtlToDatIo())
//  val dat = new DatToCtlIo()
//  val imem = new MemPortIo(conf.xprlen)
//  val reset_vector = Input(UInt())
//
//  val debug_out = Output(UInt(32.W))
//}
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
//  val io = IO(new BorgDpathIo())
//  io := DontCare
//
//  val pc_plus4 = Wire(UInt(32.W))
//
//  val pc_next = Wire(UInt(32.W))
//  pc_next := pc_plus4
//
//  // The program counter
//  val pc_reg = RegInit(io.reset_vector)
//  when (true.B) { // No stall
//    pc_reg := pc_next
//  }
//  // Get the counter out for testing
//  io.debug_out := pc_reg
//
//  pc_plus4 := (pc_reg + 4.asUInt(32.W))
//
//  io.imem.req.bits.addr := pc_reg
//  io.imem.req.valid := true.B
//
//  val regfile = Mem(32, UInt(conf.xprlen.W))
//
//  val inst = io.imem.resp.bits.data
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

class BorgCore(implicit val p: Parameters) extends Module
{
  val io = IO(new BorgCoreIo())
//  //io := DontCare
  val c  = Module(new BorgControlPath())
  val d  = Module(new BorgDataPath())

  // TODO
  //io.imem.resp.valid := DontCare
  //io.imem.resp.bits := DontCare
  //io.imem.req.valid := DontCare
  //io.imem.req.bits.addr := DontCare
//  io.debug_out := d.io.debug_out
//
//  // TMP
//  d.io.imem.resp := DontCare
//
//  // Connect the control unit to the data path unit
//  // For example the control unit decodes an instruction and informs the data path unit
//  // about the alu function
//  c.io.ctl <> d.io.ctl
//  c.io.dat <> d.io.dat
//
//  io.imem <> d.io.imem
//
//  d.io.reset_vector := io.reset_vector
}
