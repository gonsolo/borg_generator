package borg

import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy.{AddressSet, IdRange}
import freechips.rocketchip.prci._
import freechips.rocketchip.resources._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{InModuleBody, LazyModule}

case class BorgParams(
  address: BigInt = 0x4000,
  size: BigInt = 0x1000,
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

trait CanHavePeripheryBorg { this: BaseSubsystem =>
  private val portName = "borgPort"
  private val pbus = locateTLBusWrapper(PBUS)

  val borg_busy = p(BorgKey) match {
    case Some(params) => {
      val borg = {
        val borg = LazyModule(new BorgTileLink(params, pbus.beatBytes)(p))
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


