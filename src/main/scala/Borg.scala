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

//class BorgDataLoader(val w: Int) extends Module {
class BorgMMIOChiselModule(val w: Int) extends Module {
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

class BorgTL(params: BorgParams, beatBytes: Int)(implicit p: Parameters)
  extends ClockSinkDomain(ClockSinkParameters())(p) {

  val device = new SimpleDevice("borg-device", Seq("borg,borg-1"))
  val registerNode = TLRegisterNode(
    Seq(AddressSet(params.regAddress, 4096-1)),
    device,
    "reg/control",
    beatBytes=beatBytes)
  val clientNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "borg-dma-test",
    sourceId = IdRange(0, 1))))))

  override lazy val module = new BorgImpl(this)

  class BorgImpl(outer: BorgTL) extends Impl {

    // DMA
    val (tl_out, edge) = outer.clientNode.out(0)

    val (legal, get_bundle) = edge.Get(0.U, params.dmaAddress.U, log2Ceil(params.dmaBytes).U)

    // Management
    withClockAndReset(clock, reset) {

      // 0: idle or running
      // 1: done
      val status = Wire(UInt(1.W))

      //val x = Reg(UInt(params.width.W))
      // kick.valid is set on write
      val kick = Wire(new DecoupledIO(Bool())) 
      //val y = Wire(new DecoupledIO(UInt(params.width.W)))
      //val borg_result = Wire(new DecoupledIO(UInt(params.width.W)))

      val data_loader_io = {
        val data_loader= Module(new BorgDataLoader(params.width))
        data_loader.io
      }

      data_loader_io.clock := clock
      data_loader_io.reset := reset.asBool

      data_loader_io.start := kick.valid

      //impl_io.x := x
      //impl_io.y := y.bits
      //impl_io.input_valid := y.valid
      //y.ready := impl_io.input_ready

      //borg_result.bits := impl_io.borg_result
      //borg_result.valid := impl_io.output_valid
      //impl_io.output_ready := borg_result.ready

      //status := Cat(impl_io.input_ready, impl_io.output_valid)
      status := data_loader_io.done

      registerNode.regmap(
        0x00 -> Seq(RegField.r(1, status)),
        0x04 -> Seq(RegField.w(1, kick)),     // set on write
        //0x08 -> Seq(RegField.w(params.width, y)),
        //0x0C -> Seq(RegField.r(params.width, borg_result))
      )
    }
  }
}

trait CanHavePeripheryBorg { this: BaseSubsystem =>
  private val portName = "borgPort"
  private val pbus = locateTLBusWrapper(PBUS)

  p(BorgKey) match {
    case Some(params) => {
      val borg = {
        val borg = LazyModule(new BorgTileLink(params, pbus.beatBytes)(p))
        borg.clockNode := pbus.fixedClockNode
        pbus.coupleTo(portName) { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
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

