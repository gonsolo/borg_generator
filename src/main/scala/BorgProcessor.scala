// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import Constants._

object Instructions
{
  val LUI  = BitPat("b?????????????????????????0110111")
  val LW   = BitPat("b?????????????????010?????0000011")
  val SW   = BitPat("b?????????????????010?????0100011")
  val ADDI = BitPat("b?????????????????000?????0010011")
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

    when(LW.matches(instruction))         { kind := 1.U }
    .elsewhen(LUI.matches(instruction))   { kind := 2.U }
    .elsewhen(SW.matches(instruction))    { kind := 3.U }
    .elsewhen(ADDI.matches(instruction))  { kind := 4.U }
    .elsewhen(BUBBLE === instruction)     { kind := 5.U }

    val table = VecInit(Seq(
      VecInit("UNKN".map(_.U(8.W))),
      VecInit("  LW".map(_.U(8.W))),
      VecInit(" LUI".map(_.U(8.W))),
      VecInit("  SW".map(_.U(8.W))),
      VecInit("ADDI".map(_.U(8.W))),
      VecInit("BUBB".map(_.U(8.W)))
    ))

    val chars = table(kind)
    cf"${chars(0)}%c${chars(1)}%c${chars(2)}%c${chars(3)}%c"
  }
}

// Signals from the control unit to the data path unit
class CtlToDatIo() extends Bundle() {

  // The CPU is stalled when waiting for the instruction cache. The program counter is not updated then.
  val stall = Output(Bool())

  // The control unit decodes the instruction and set the corresponding alu function for the data path unit.
  val alu_fun = Output(UInt(ALU_X.getWidth.W))

  val operand1_select = Output(UInt(OP1_X.getWidth.W))
  val wb_sel = Output(UInt(WB_X.getWidth.W))
  val rf_wen = Output(Bool())
  val stored_rd = Output(UInt(32.W))
}

class BorgCoreModule(outer: BorgCore) extends LazyModuleImp(outer)
{
  val io = IO(new BorgCoreIo())
  io := DontCare

  val c  = Module(new BorgControlPath())
  c.io.imem.request := DontCare

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

  val dataCache = outer.dataCache.module
  dataCache.reset := reset
  d.io.dmem <> dataCache.io
  c.io.dmem <> dataCache.io
}

class BorgCore()(implicit p: Parameters) extends LazyModule
{
  lazy val module = new BorgCoreModule(this)
  val instructionCache = LazyModule(new BorgInstructionCache())
  val dataCache = LazyModule(new BorgDataCache())
}

