package borg

import scala.collection.immutable.ArraySeq.unsafeWrapArray

import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy.{AddressSet, IdRange}
import freechips.rocketchip.prci._
import freechips.rocketchip.resources._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
//import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{InModuleBody, LazyModule}

case class BorgParams(
  address: BigInt = 0x4000,
  width: Int = 32)

case object BorgKey extends Field[Option[BorgParams]](None)

class BorgIO(val w: Int) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val input_ready = Output(Bool())
  val input_valid = Input(Bool())
  val x = Input(UInt(w.W))
  val y = Input(UInt(w.W))
  val output_ready = Input(Bool())
  val output_valid = Output(Bool())
  val borg_result = Output(UInt(w.W))
  val busy = Output(Bool())
}

class BorgTopIO() extends Bundle {
  val borg_busy = Output(Bool())
}

trait HasBorgTopIO {
  def io: BorgTopIO
}

class BorgMMIOChiselModule(val w: Int) extends Module {
  val io = IO(new BorgIO(w))
  val s_idle :: s_run :: s_done :: Nil = Enum(3)

  val state         = RegInit(s_idle)
  val tmp           = Reg(UInt(w.W))
  val borg_result   = Reg(UInt(w.W))

  io.input_ready := state === s_idle
  io.output_valid := state === s_done
  io.borg_result := borg_result

  when (state === s_idle && io.input_valid) {
    state := s_run
  } .elsewhen (state === s_run && tmp === 0.U) {
    state := s_done
  } .elsewhen (state === s_done && io.output_ready) {
    state := s_idle
  }

  when (state === s_idle && io.input_valid) {
    borg_result := io.x
    tmp := io.y
  } .elsewhen (state === s_run) {
    when (borg_result > tmp) {
      borg_result := borg_result - tmp
    } .otherwise {
      tmp := tmp - borg_result
    }
  }

  io.busy := state =/= s_idle
}

class BorgTL(params: BorgParams, beatBytes: Int)(implicit p: Parameters)
  extends ClockSinkDomain(ClockSinkParameters())(p) {

  val device = new SimpleDevice("borg-device", Seq("borg,borg-1"))
  val registerNode = TLRegisterNode(
    Seq(AddressSet(params.address, 4096-1)),
    device,
    "reg/control",
    beatBytes=beatBytes)
  val clientNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "dma-test",
    sourceId = IdRange(0, 1))))))

  override lazy val module = new BorgImpl

  class BorgImpl extends Impl with HasBorgTopIO {
    val io = IO(new BorgTopIO)
    withClockAndReset(clock, reset) {
      val x = Reg(UInt(params.width.W))
      val y = Wire(new DecoupledIO(UInt(params.width.W)))
      val borg_result = Wire(new DecoupledIO(UInt(params.width.W)))
      val status = Wire(UInt(2.W))

      val impl_io = {
        val impl = Module(new BorgMMIOChiselModule(params.width))
        impl.io
      }

      impl_io.clock := clock
      impl_io.reset := reset.asBool

      impl_io.x := x
      impl_io.y := y.bits
      impl_io.input_valid := y.valid
      y.ready := impl_io.input_ready

      borg_result.bits := impl_io.borg_result
      borg_result.valid := impl_io.output_valid
      impl_io.output_ready := borg_result.ready

      status := Cat(impl_io.input_ready, impl_io.output_valid)
      io.borg_busy := impl_io.busy

      registerNode.regmap(
        0x00 -> Seq(RegField.r(2, status)),
        0x04 -> Seq(RegField.w(params.width, x)),
        0x08 -> Seq(RegField.w(params.width, y)),
        0x0C -> Seq(RegField.r(params.width, borg_result))
      )
    }
  }
}

trait CanHavePeripheryBorg { this: BaseSubsystem =>
  private val portName = "borgPort"
  private val pbus = locateTLBusWrapper(PBUS)

  val borg_busy = p(BorgKey) match {
    case Some(params) => {
      val borg = {
        val borg = LazyModule(new BorgTL(params, pbus.beatBytes)(p))
        borg.clockNode := pbus.fixedClockNode
        pbus.coupleTo(portName) { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
        borg
      }
      val borg_busy = InModuleBody {
        val busy = IO(Output(Bool())).suggestName("borg_busy")
        busy := borg.module.io.borg_busy
        busy
      }
      Some(borg_busy)
    }
    case None => None
  }
}

class WithBorg() extends Config((site, here, up) => {
  case BorgKey => {
    Some(BorgParams())
  }
})

// --------------------- Experimental Processor -------------------------------------------------//

class BorgCoreIo(implicit val p: Parameters, val conf: BorgCoreParams) extends Bundle
{
  // TODO
}

case class BorgCoreParams(
  xprlen: Int = 32
) {}

class DatToCtlIo() extends Bundle() {
  val inst   = Output(UInt(32.W))
}

trait ScalarOpConstants
{
  // ALU Operation Signal
  val ALU_X   = 0.asUInt(4.W) // unspecified alu function
  val ALU_ADD = 1.asUInt(4.W) // add alu function
}

object Constants extends ScalarOpConstants with RISCVConstants {}

import Constants._

class BorgControlPathIo() extends Bundle() {
  val dat = Flipped(new DatToCtlIo())
  val ctl = new CtlToDatIo()
}

object Instructions
{
  def ADDI               = BitPat("b?????????????????000?????0010011")
}

import Instructions._

class BorgControlPath(implicit val conf: BorgCoreParams) extends Module
{
  // Input and output signals for the control unit
  val io = IO(new BorgControlPathIo())
  io := DontCare

  // Look up the incoming instruction and set the ALU operation accordingly
  val csignals = ListLookup(
    io.dat.inst,
    List(              ALU_X),
    Array(
    // instruction   | alu function
      ADDI    -> List( ALU_ADD)
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
  val alu_fun   = Output(UInt(ALU_X.getWidth.W))
}

class MemResp(data_width: Int) extends Bundle
{
  val data = Output(UInt(data_width.W))
}

class MemPortIo(data_width: Int) extends Bundle
{
  val resp = Flipped(new ValidIO(new MemResp(data_width)))
}

class DpathIo(implicit val conf: BorgCoreParams) extends Bundle()
{
  val ctl = Flipped(new CtlToDatIo())
  val dat = new DatToCtlIo()
  val imem = new MemPortIo(conf.xprlen)
}

trait RISCVConstants {
  val RD_MSB = 11
  val RD_LSB = 7
  val RS1_MSB = 19
  val RS1_LSB = 15
}

class BorgDataPath(implicit val p: Parameters, val conf: BorgCoreParams) extends Module
{
  val io = IO(new DpathIo())
  io := DontCare

  val regfile = Mem(32, UInt(conf.xprlen.W))

  val inst = io.imem.resp.bits.data

  val rs1_addr = inst(RS1_MSB, RS1_LSB)

  val rs1_data = regfile(rs1_addr)

  // immediates
  val imm_i = inst(31, 20)

  // sign-extend immediates
  val imm_i_sext = Cat(Fill(20,imm_i(11)), imm_i)

  // For now: ADDI is always register source 1
  val alu_op1 = rs1_data

  // For now: ADDI is always immediate
  val alu_op2 = imm_i_sext

  val alu_out = Wire(UInt(conf.xprlen.W))

  alu_out := MuxCase(0.U, unsafeWrapArray(Array(
      (io.ctl.alu_fun === ALU_ADD) -> (alu_op1 + alu_op2).asUInt
    )))

  val wb_data = Wire(UInt(conf.xprlen.W))

  wb_data := alu_out

  // Writeback write enable
  val wb_wen = true.B // TODO

  // The address to write back to
  val wb_addr = inst(RD_MSB, RD_LSB)

  when (wb_wen && (wb_addr =/= 0.U))
  {
    regfile(wb_addr) := wb_data
  }

  // To control unit
  io.dat.inst := inst
}

class BorgCore(implicit val p: Parameters, val conf: BorgCoreParams)
{
  val io = IO(new BorgCoreIo())
  io := DontCare
  val c  = Module(new BorgControlPath())
  val d  = Module(new BorgDataPath())

  // Connect the control unit to the data path unit
  // For example the control unit decodes an instruction and informs the data path unit
  // about the alu function
  c.io.ctl <> d.io.ctl
}
