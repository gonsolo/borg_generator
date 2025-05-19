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
}

object Instructions
{
  def ADDI               = BitPat("b?????????????????000?????0010011")
}

import Instructions._

class BorgControlPath() extends Module
{
  // Input and output signals for the control unit
  val io = IO(new BorgControlPathIo())

  // Look up the incoming instruction and set the ALU operation accordingly
  val csignals = ListLookup(
    io.dat.instruction,
    List(              ALU_X),
    Array(
    // instruction  | alu function
      ADDI          -> List(ALU_ADD)
    )
  )

  // Put the alu function into a variable
  val cs_alu_fun :: Nil = csignals

  // Set the data path control signals
  io.ctl.alu_fun := cs_alu_fun
}

// Signals from the control unit to the data path unit
class CtlToDatIo() extends Bundle() {

  // The control unit decodes the instruction and set the correspong alu function for the data path unit
  val alu_fun = Output(UInt(ALU_X.getWidth.W))
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
  programCounter := Mux(reset.asBool, io.startAddress, programCounter + 4.U)

  io.imem.request.bits.address := programCounter
  io.imem.request.bits.function := M_XREAD
  io.imem.request.bits.data := DontCare
  io.imem.request.valid := Mux(reset.asBool, false.B, true.B)
  printf(cf"BorgDataPath request valid: ${io.imem.request.valid} address: 0x${io.imem.request.bits.address}%x\n")
  io.imem.request.ready := DontCare

  val instruction = Mux(io.imem.response.valid, io.imem.response.bits.data, BUBBLE)
  printf(cf"Borg instruction: 0x$instruction%x\n")

  val regfile = Mem(32, UInt(64.W))

  // Decode
  val rs1_addr = instruction(RS1_MSB, RS1_LSB)

  val rs1_data = regfile(rs1_addr)

  // immediates
  val imm_i = instruction(31, 20)
  printf(cf" immediate: $imm_i\n")

  // sign-extend immediates
  val imm_i_sext = Cat(Fill(20,imm_i(11)), imm_i)

  // For now: ADDI is always register source 1
  val alu_op1 = rs1_data

  // For now: ADDI is always immediate
  val alu_op2 = imm_i_sext

  val alu_out = Wire(UInt(64.W))

  alu_out := MuxCase(0.U, unsafeWrapArray(Array(
      (io.ctl.alu_fun === ALU_ADD) -> (alu_op1 + alu_op2).asUInt
    )))
  printf(cf" alu_out: $alu_out\n")

  val wb_data = Wire(UInt(64.W))

  wb_data := alu_out
  printf(cf" wb_data: $wb_data\n")

  // Writeback write enable
  val wb_wen = true.B // TODO

  // The address to write back to
  val wb_addr = instruction(RD_MSB, RD_LSB)
  printf(cf" wb_addr: $wb_addr\n")

  when (wb_wen && (wb_addr =/= 0.U))
  {
    printf(" writing\n")
    regfile(wb_addr) := wb_data
  }

  val address_written = RegNext(wb_addr)
  printf(cf"  regfile wb_addr: ${regfile(address_written)}\n")

  // To control unit
  io.dat.instruction := instruction
}

class BorgCoreModule(outer: BorgCore) extends LazyModuleImp(outer)
{
  val io = IO(new BorgCoreIo())
  io := DontCare

  val c  = Module(new BorgControlPath())

  val d  = Module(new BorgDataPath())
  d.reset := reset
  d.io.startAddress := io.startAddress
  d.io.ctl <> c.io.ctl
  d.io.dat <> c.io.dat

  val instructionCache = outer.instructionCache.module
  instructionCache.reset := reset
  instructionCache.io.request <> d.io.imem.request
  d.io.imem.response <> instructionCache.io.response

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
}

