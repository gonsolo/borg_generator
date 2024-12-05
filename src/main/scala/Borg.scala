package borg

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

case class BorgCoreParams() {}

class DatToCtlIo() extends Bundle() {
  val inst   = Output(UInt(32.W))
}

class BorgControlPathIo() extends Bundle() {
  val dat  = Flipped(new DatToCtlIo())
}

trait MemoryOpConstants
{
  val MT_X  = 0.asUInt(3.W)

  val M_X   = "b0".asUInt(1.W)
}

trait ScalarOpConstants
{
  val Y = true.B
  val N = false.B

  val BR_N   = 0.asUInt(4.W)

  // RS1 Operand Select Signal
  val OP1_RS1 = 0.asUInt(2.W) // Register Source #1
  val OP1_X   = 0.asUInt(2.W)

  // RS2 Operand Select Signal
  val OP2_IMI = 1.asUInt(2.W) // immediate, I-type

  val OP2_X   = 0.asUInt(2.W)

  // Register File Write Enable Signal
  val REN_0   = false.B
  val REN_1   = true.B
  val REN_X   = false.B

  // ALU Operation Signal
  val ALU_ADD = 1.asUInt(4.W)
  val ALU_X   = 0.asUInt(4.W)

  // Writeback Select Signal
  val WB_ALU  = 0.asUInt(2.W)

  val WB_X    = 0.asUInt(2.W)

  val MEN_0   = false.B

}

object Constants extends ScalarOpConstants with MemoryOpConstants {}

import Constants._

object CSR
{
  val SZ = 3
  def N = 0.U(SZ.W)
}

object Instructions
{
  def ADDI               = BitPat("b?????????????????000?????0010011")
}

import Instructions._

class BorgControlPath(implicit val conf: BorgCoreParams) extends Module
{
  val io = IO(new BorgControlPathIo())
  io := DontCare

  val csignals = ListLookup(
    io.dat.inst,
    List(N, BR_N  , OP1_X  ,  OP2_X  , ALU_X   , WB_X   , REN_0, MEN_0, M_X  , MT_X,  CSR.N),
    Array(
      ADDI    -> List(Y, BR_N  , OP1_RS1, OP2_IMI , ALU_ADD ,  WB_ALU, REN_1, MEN_0, M_X  , MT_X,  CSR.N)
    )
  )
}

class BorgDataPath(implicit val p: Parameters, val conf: BorgCoreParams) extends Module
{
  // TODO
}

class BorgCore(implicit val p: Parameters, val conf: BorgCoreParams)
{
  val io = IO(new BorgCoreIo())
  io := DontCare
  val c  = Module(new BorgControlPath())
  val d  = Module(new BorgDataPath())
}
