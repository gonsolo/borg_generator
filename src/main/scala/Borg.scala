// Copyright Andreas Wendleder 2025
// GPL-3.0-only

package borg

import chisel3._
import chisel3.util.{Enum, log2Ceil}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange}
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes, PBUS}
import freechips.rocketchip.regmapper.{RegField}
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.tilelink.{TLClientNode, TLFragmenter, TLMasterParameters, TLMasterPortParameters, TLRegisterNode}
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

case class BorgConfig()

case object BorgKey extends Field[Option[BorgConfig]](None)

class Borg(beatBytes: Int)(implicit p: Parameters) extends LazyModule {

  val regAddress: BigInt = 0x4000
  val regSize: BigInt = 0x0FFF

  val device = new SimpleDevice("borg-device", Seq("borg,borg-1"))
  val registerNode = TLRegisterNode(Seq(AddressSet(regAddress, regSize)), device, "reg/control", beatBytes=beatBytes)
  val dmaNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "borg-dma", sourceId = IdRange(0, 1))))))

  lazy val module = new BorgModuleImp(this)
}

class BorgLoaderIO extends Bundle {
  val kick = Input(UInt(32.W))
  val seen = Output(UInt(32.W))
}

class BorgLoader(outer: Borg, blockBytes: Int) extends Module {
  val io = IO(new BorgLoaderIO())

  val seen = RegInit(0.U(32.W))
  when (io.kick === 1.U) {
    seen := 1.U
  }
  io.seen := seen
}

class BorgModuleImp(outer: Borg) extends LazyModuleImp(outer) {

  val blockBytes = p(CacheBlockBytes)

  val test1 = RegInit(666.U(32.W))

  val kick = RegInit(0.U(32.W))
  when (kick === 1.U) {
    kick := 0.U
  }

  val loader = Module(new BorgLoader(outer, blockBytes))
  loader.io.kick := kick

  // When kick equals 1 start DMA download
  // Writing for now, reading later
  val (mem, edge) = outer.dmaNode.out(0)
  val addressBits = edge.bundle.addressBits
  val dmaBase = 0x88000000L
  val dmaSize = 0x100L
  require(dmaSize % blockBytes == 0)

  val s_init :: s_write:: s_resp :: s_done :: Nil = Enum(4)
  val state = RegInit(s_init)
  val address = Reg(UInt(addressBits.W))
  val bytesLeft = Reg(UInt(log2Ceil(dmaSize+1).W))

  mem.a.valid := state === s_write
  mem.a.bits := edge.Put(
    fromSource = 0.U,
    toAddress = address,
    lgSize = log2Ceil(blockBytes).U,
    data = 999.U)._2
  mem.d.ready := state === s_resp

  when (state === s_init && kick === 1.U) {
    address := dmaBase.U
    bytesLeft := dmaSize.U
    state := s_write
  }
  when (edge.done(mem.a)) {
    address := address + blockBytes.U
    bytesLeft := bytesLeft - blockBytes.U
    state := s_resp
  }
  when (mem.d.fire) {
    state := Mux(bytesLeft === 0.U, s_done, s_write)
  }

  val done = RegInit(0.U(32.W))
  when (state === s_done) {
    done := 1.U
  }

  outer.registerNode.regmap(
    0x00 -> Seq(RegField.r(32, test1)),
    0x20 -> Seq(RegField.r(32, kick)),
    0x40 -> Seq(RegField.w(32, kick)),
    0x60 -> Seq(RegField.r(32, loader.io.seen)),
    0x80 -> Seq(RegField.r(32, done)),
  )
}

trait CanHavePeripheryBorg { this: BaseSubsystem =>
  implicit val p: Parameters

  p(BorgKey) .map { k =>
    val pbus = locateTLBusWrapper(PBUS)
    val borg = pbus { LazyModule(new Borg(pbus.beatBytes)(p)) }
    pbus.coupleTo("borg-borg") { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
    pbus.coupleFrom("borg-dma") { _ := borg.dmaNode }
  }
}

class WithBorg() extends Config((site, here, up) => {
  case BorgKey => Some(BorgConfig())
})
