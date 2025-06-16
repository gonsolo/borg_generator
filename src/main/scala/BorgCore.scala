// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import Constants._
import ALU._

object Instructions
{
  //val LUI     = BitPat("b?????????????????????????0110111") // Load Upper Immediate
  //val LW      = BitPat("b?????????????????010?????0000011") // Load Word
  //val SW      = BitPat("b?????????????????010?????0100011") // Store Word

  val AUIPC   = BitPat("b?????????????????????????0010111") // Add Upper Immediate to Program Counter
  val ADDI    = BitPat("b?????????????????000?????0010011") // ADD Immediate
  val FLW     = BitPat("b?????????????????010?????0000111") // Floating point Load Word
  val FADDS   = BitPat("b0000000??????????000?????1010011") // Floating point ADD Single precision
  val FSW     = BitPat("b?????????????????010?????0100111") // Floating point Store Word
}

import Instructions._

class BorgCoreIo() extends Bundle
{
  val imem = new MemoryPortIo()
  val startAddress = Input(UInt(32.W))
}

class DatToCtlIo() extends Bundle {
  val instruction = Output(UInt(32.W))

  override def toPrintable: Printable = {
    val kind = Wire(UInt(3.W))
    kind := 0.U // default

    when(AUIPC.matches(instruction))        { kind := 1.U }
    .elsewhen(ADDI.matches(instruction))    { kind := 2.U }
    .elsewhen(FLW.matches(instruction))     { kind := 3.U }
    .elsewhen(FADDS.matches(instruction))   { kind := 4.U }
    .elsewhen(FSW.matches(instruction))     { kind := 5.U }
    .elsewhen(BUBBLE === instruction)       { kind := 6.U }

    val table = VecInit(Seq(
      VecInit("UNKNO".map(_.U(8.W))),
      VecInit("AUIPC".map(_.U(8.W))),
      VecInit(" ADDI".map(_.U(8.W))),
      VecInit("  FLW".map(_.U(8.W))),
      VecInit("FADDS".map(_.U(8.W))),
      VecInit("  FSW".map(_.U(8.W))),
      VecInit("BUBB".map(_.U(8.W)))
    ))

    val chars = table(kind)
    cf"${chars(0)}%c${chars(1)}%c${chars(2)}%c${chars(3)}%c${chars(4)}%c"
  }

  val sodor_csr_eret = Output(Bool())
}

// Signals from the control unit to the data path unit
class CtlToDatIo() extends Bundle() {

  // The CPU is stalled when waiting for the instruction cache. The program counter is not updated then.
  val stall = Output(Bool())

  // The control unit decodes the instruction and set the corresponding alu function for the data path unit.
  //val alu_fun = Output(UInt(ALU_X.getWidth.W))

  val operand1_select = Output(UInt(OP1_X.getWidth.W))
  //val wb_sel = Output(UInt(WB_X.getWidth.W))
  //val rf_wen = Output(Bool())
  val stored_rd = Output(UInt(32.W))

  val exe_kill      = Output(Bool())
  val pc_sel        = Output(UInt(3.W))
  val brjmp_sel     = Output(Bool())
  val op1_sel       = Output(UInt(2.W))
  val op2_sel       = Output(UInt(2.W))
  val alu_fun       = Output(UInt(SZ_ALU_FN.W))
  val wb_sel        = Output(UInt(2.W))
  val rf_wen        = Output(Bool())
  val bypassable    = Output(Bool())
  val csr_cmd       = Output(UInt(SODOR_CSR_SZ))
  val dmem_val      = Output(Bool())
  val dmem_function = Output(UInt(MEMORY_X.getWidth.W))
  val dmem_typ      = Output(UInt(3.W))
  val exception     = Output(Bool())
}

class BorgCoreModule(outer: BorgCore) extends LazyModuleImp(outer)
{
  val io = IO(new BorgCoreIo())
  io := DontCare

  val instructionCache = outer.instructionCache.module
  val dataCache = outer.dataCache.module
  val frontEnd = Module(new FrontEnd())
  val controlPath  = Module(new ControlPath())
  val dataPath  = Module(new DataPath())

  frontEnd.io.imem <> instructionCache.io
  frontEnd.io.cpu <> controlPath.io.imem
  frontEnd.io.cpu <> dataPath.io.imem
  frontEnd.io.cpu.request.valid := controlPath.io.imem.request.valid

  controlPath.io.ctl <> dataPath.io.ctl
  controlPath.io.dat <> dataPath.io.dat

  controlPath.io.dmem <> dataCache.io
  dataPath.io.dmem <> dataCache.io

  dataPath.io.startAddress := io.startAddress

  instructionCache.reset := reset
  dataCache.reset := reset
}

class BorgCore()(implicit p: Parameters) extends LazyModule
{
  lazy val module = new BorgCoreModule(this)
  val instructionCache = LazyModule(new BorgInstructionCache())
  val dataCache = LazyModule(new BorgDataCache())
}

