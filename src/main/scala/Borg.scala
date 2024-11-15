package borg

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam, BaseModule}

import freechips.rocketchip.diplomacy.{AddressSet, IdRange}
import freechips.rocketchip.interrupts.IntIdentityNode
import freechips.rocketchip.prci._
import freechips.rocketchip.resources._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{InModuleBody, LazyModule}

case class BorgParams(
  address: BigInt = 0x4000,
  width: Int = 32,
  dmaBase: BigInt = 0x88000000L,
  dmaSize: BigInt = 0x1000)

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
  val borg = Output(UInt(w.W))
  val busy = Output(Bool())
}

class BorgTopIO extends Bundle {
  //val borg_busy = Output(Bool())
}

trait HasBorgTopIO {
  def io: BorgTopIO
}

class BorgMMIOChiselModule(val w: Int) extends Module {
  val io = IO(new BorgIO(w))
  val s_idle :: s_run :: s_done :: Nil = Enum(3)

  val state = RegInit(s_idle)
  val tmp   = Reg(UInt(w.W))
  val borg   = Reg(UInt(w.W))

  io.input_ready := state === s_idle
  io.output_valid := state === s_done
  io.borg := borg

  when (state === s_idle && io.input_valid) {
    state := s_run
  } .elsewhen (state === s_run && tmp === 0.U) {
    state := s_done
  } .elsewhen (state === s_done && io.output_ready) {
    state := s_idle
  }

  when (state === s_idle && io.input_valid) {
    borg := io.x
    tmp := io.y
  } .elsewhen (state === s_run) {
    when (borg > tmp) {
      borg := borg - tmp
    } .otherwise {
      tmp := tmp - borg
    }
  }

  io.busy := state =/= s_idle
}

class BorgTL(params: BorgParams, beatBytes: Int)(implicit p: Parameters) extends ClockSinkDomain(ClockSinkParameters())(p) {

  val device = new SimpleDevice("borg-device", Seq("borg,borg-1"))
  val registerNode = TLRegisterNode(Seq(AddressSet(params.address, 4096-1)), device, "reg/control", beatBytes=beatBytes)
  val clientNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "dma-test", sourceId = IdRange(0, 1))))))

  override lazy val module = new BorgImpl

  class BorgImpl extends Impl with HasBorgTopIO {

    val io = IO(new BorgTopIO)

    // DMA
    val (mem, edge) = clientNode.out(0)
    val addressBits = edge.bundle.addressBits
    val blockBytes = p(CacheBlockBytes)
    require(params.dmaSize % blockBytes == 0, "DMA size does not match!")
    val s_dma_init :: s_dma_write :: s_dma_resp :: s_dma_done :: Nil = Enum(4)

    withClockAndReset(clock, reset) {
      val dma_state = RegInit(s_dma_init)
      val address = Reg(UInt(addressBits.W))
      val bytesLeft = Reg(UInt(log2Ceil(params.dmaSize+1).W))
      val testValue = 666.U
      val lgBlockBytes = log2Ceil(blockBytes).U

      mem.a.valid := dma_state === s_dma_write
      val (_, putBits) = edge.Put(
        fromSource = 0.U,
        toAddress = address,
        lgSize = lgBlockBytes,
        data = testValue)
      mem.d.ready := dma_state === s_dma_resp

      //val (_, getBits) = edge.Get(
      //  fromSource = 0.U,
      //  toAddress = address,
      //  lgSize = lgBlockBytes)

      //val putting = true.B
      //when (putting) {
      //  mem.a.bits := putBits
      //}.otherwise {
      //  mem.a.bits := getBits
      //}
      mem.a.bits := putBits

      when (dma_state === s_dma_init) {
        address := params.dmaBase.U
        bytesLeft := params.dmaSize.U
        dma_state := s_dma_write
      }

      when (edge.done(mem.a)) {
        address := address + blockBytes.U
        bytesLeft := bytesLeft - blockBytes.U
        dma_state := s_dma_resp
      }

      when (mem.d.fire) {
        dma_state := Mux(bytesLeft === 0.U, s_dma_done, s_dma_write)
      }
      // DMA

      //val x = Reg(UInt(params.width.W))
      //val y = Wire(new DecoupledIO(UInt(params.width.W)))
      //val borg = Wire(new DecoupledIO(UInt(params.width.W)))
      //val status = Wire(UInt(2.W))

      //val impl_io = {
      //  val impl = Module(new BorgMMIOChiselModule(params.width))
      //  impl.io
      //}

      //impl_io.clock := clock
      //impl_io.reset := reset.asBool

      //impl_io.x := x
      //impl_io.y := y.bits
      //impl_io.input_valid := y.valid
      //y.ready := impl_io.input_ready

      //borg.bits := impl_io.borg
      //borg.valid := impl_io.output_valid
      //impl_io.output_ready := borg.ready

      //status := Cat(impl_io.input_ready, impl_io.output_valid)
      //io.borg_busy := impl_io.busy

      registerNode.regmap(
        //0x00 -> Seq(
        //  RegField.r(2, status)),               // a read-only status register
        //0x04 -> Seq(
        //  RegField.w(params.width, x)),         // a plain, write-only register
        //0x08 -> Seq(
        //  RegField.w(params.width, y)),         // write-only, y.valid is set on write
        //0x0C -> Seq(
        //  RegField.r(params.width, borg)),       // read-only, borg.ready is set on read
        0x10 -> Seq(
          RegField.r(2, dma_state))             // read-only
        )
    }
  }
}

class BorgTile(
  params: RocketTileParams,
  crossing: HierarchicalElementCrossingParamsLike,
  lookup: LookupByHartIdImpl,
  q: Parameters)
  extends RocketTile(params, crossing, lookup)(q) {

    override val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("sifive,rocket0", "riscv")) {
      override def parent = Some(ResourceAnchors.soc)
    }
}

trait CanHavePeripheryBorg { this: BaseSubsystem =>
  private val portName = "borgPort"
  private val pbus = locateTLBusWrapper(PBUS)
  private val fbus = locateTLBusWrapper(FBUS)

  val borg_busy = p(BorgKey) match {
    case Some(params) => {
      val borg = {
        val borg = LazyModule(new BorgTL(params, pbus.beatBytes)(p))
        borg.clockNode := pbus.fixedClockNode
        pbus.coupleTo(portName) { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
        fbus.coupleFrom("dma-test") { _ := borg.clientNode }
        borg
      }
      val borg_busy = InModuleBody {
        //val busy = IO(Output(Bool())).suggestName("borg_busy")
        //busy := borg.module.io.borg_busy
        //busy
      }
      Some(borg_busy)
    }
    case None => None
  }
}

class WithBorg() extends Config((site, here, up) => {
  case BorgKey => Some(BorgParams())
})
