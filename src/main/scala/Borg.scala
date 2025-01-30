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
  regAddress: BigInt = 0x4000,
  dmaAddress: BigInt = 0x5000,
  dmaBytes: BigInt = 8,
  size: BigInt = 0x1000,
  width: Int = 32)

case object BorgKey extends Field[Option[BorgParams]](None)

class BorgIO(val w: Int) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())

  val start = Input(Bool())

  val done = Output(Bool())

  //val input_ready = Output(Bool())
  //val input_valid = Input(Bool())
  //val x = Input(UInt(w.W))
  //val y = Input(UInt(w.W))
  //val output_ready = Input(Bool())
  //val output_valid = Output(Bool())
  //val borg_result = Output(UInt(w.W))
}

class BorgDataLoader(val w: Int) extends Module {
  val io = IO(new BorgIO(w))
  val s_idle :: s_run :: Nil = Enum(2)
  val state = RegInit(s_idle)

  when (state === s_idle && io.start) {
    state := s_run
  } .elsewhen (state === s_run && io.done) {
    state := s_idle
  }

  when (state === s_run) {
    // TODO
  }

  io.done := state === s_idle
}

trait CanHavePeripheryBorg { this: BaseSubsystem =>
  private val pbus = locateTLBusWrapper(PBUS)

  p(BorgKey) match {
    case Some(params) => {
      val borg = {
        val borg = LazyModule(new BorgTileLink(params, pbus.beatBytes)(p))
        //borg.clockNode := pbus.fixedClockNode
        pbus.coupleTo("borgPort") { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
        pbus.coupleFrom("borgPortDma") { _ := borg.clientNode }
        borg
      }
    }
    case None => None
  }
}

class WithBorg() extends Config((site, here, up) => {
  case BorgKey => {
    Some(BorgParams())
  }
})

