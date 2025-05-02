// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util.{Cat, Enum, is, log2Ceil, switch}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, TransferSizes}
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes, FBUS, PBUS}
import freechips.rocketchip.regmapper.{RegField}
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import scala.language.reflectiveCalls
import Constants._

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

class BorgIo extends Bundle {
  val x = Bool()
}

class BorgModuleImp(outer: Borg) extends LazyModuleImp(outer) {

  val io = IO(new BorgIo)
  io.x := true.B

  val blockBytes = p(CacheBlockBytes)
  require(blockBytes == 64)

  val test1 = RegInit(666.U(32.W))

  val kick = RegInit(0.U(32.W))
  when (kick === 1.U) {
    kick := 0.U
  }
  val completed = RegInit(false.B)
  val shaderBase = RegInit(0.U(64.W))
  val shaderSize = RegInit(0.U(32.W))

  //val (mem, edge) = outer.dmaNode.out(0)
  //val addressBits = edge.bundle.addressBits

  val dmaSize = 16 * 64 // 1024 bytes
  val instructionSize = dmaSize / 4 // instructions are 32 bit wide
  val instructionWidth = 32

  val s_idle :: s_shader :: Nil = Enum(2)
  val state = RegInit(s_idle)
  val dmaSizeWidth = log2Ceil(dmaSize+1).W
  //val bytesLeft = Reg(UInt(dmaSizeWidth))
  //val data = Reg(UInt(64.W))

  //val memoryIndex = RegInit(0.U(dmaSizeWidth))

  //val src = WireInit(0.U)
  //val address = Reg(UInt(addressBits.W))
  //val size = log2Ceil(blockBytes).U

  val core = LazyModule(new BorgCore())
  //core.io := DontCare

  printf(cf"BorgModuleImp step\n")

  //switch (state) {
  //  is (s_idle) {
  //    printf(cf"BorgModuleImp idle\n")
  //    core.module.io.reset := kick
  //    core.module.io.startAddress := shaderBase
  //  }
  //}

  outer.registerNode.regmap(
    0x000 -> Seq(RegField.r(32, test1)),
    0x020 -> Seq(RegField.w(32, kick)),
    0x040 -> Seq(RegField.r(32, completed)),
    0x060 -> Seq(RegField.w(64, shaderBase)),
    0x100 -> Seq(RegField.w(32, shaderSize)),
  )
}

trait CanHavePeripheryBorg { this: BaseSubsystem =>
  implicit val p: Parameters

  p(BorgKey) .map { k =>
    val pbus = locateTLBusWrapper(PBUS)
    val borg = pbus { LazyModule(new Borg(pbus.beatBytes)(p)) }
    pbus.coupleTo("borg-borg") { borg.registerNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
    val fbus = locateTLBusWrapper(FBUS)
    fbus.coupleFrom("borg-dma") { _ := borg.dmaNode }
  }
}

class WithBorg() extends Config((site, here, up) => {
  case BorgKey => Some(BorgConfig())
})


