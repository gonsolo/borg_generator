// Copyright Andreas Wendleder 2025
// GPL-3.0-only

package borg

import chisel3._
import chisel3.util.{Cat, Enum, is, log2Ceil, switch}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange}
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes, FBUS, PBUS}
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

class BorgModuleImp(outer: Borg) extends LazyModuleImp(outer) {

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

  val (mem, edge) = outer.dmaNode.out(0)
  val addressBits = edge.bundle.addressBits
  //val dmaBase = 0x88000000L
  val dmaSize = 16 * 64 // 1024 bytes
  //require(shaderSize % blockBytes == 0)

  val s_idle :: s_read :: s_response :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val dmaSizeWidth = log2Ceil(dmaSize+1).W
  val bytesLeft = Reg(UInt(dmaSizeWidth))
  val data = Reg(UInt(64.W))
  val memory = Mem(dmaSize, UInt(64.W))
  val memoryIndex = RegInit(0.U(dmaSizeWidth))

  val src = WireInit(0.U)
  val address = Reg(UInt(addressBits.W))
  val size = log2Ceil(blockBytes).U

  val getPutBits = edge.Get(src, address, size)._2

  mem.a.bits := getPutBits
  data := mem.d.bits.data
  val dValidSeen = RegInit(false.B)

  switch (state) {
    is (s_idle) {
      mem.a.valid := false.B
      mem.d.ready := false.B
      when (kick === 1.U) {
        //address := dmaBase.U
        address := shaderBase
        bytesLeft := dmaSize.U
        memoryIndex := 0.U
        completed := false.B
        state := s_read
      }
      //for ( i <- 0 to 15) { printf(cf"Borg memory $i: 0x${memory(i)}%x\n") }
    }
    is (s_read) {
      mem.a.valid := true.B
      mem.d.ready := false.B
      when (edge.done(mem.a)) {
        address := address + blockBytes.U
        bytesLeft := bytesLeft - blockBytes.U
        state := s_response
      }
    }
    is (s_response) {
      mem.a.valid := false.B
      mem.d.ready := true.B
      when (mem.d.valid === true.B) {
        dValidSeen := true.B
        memory(memoryIndex) := mem.d.bits.data
        memoryIndex := memoryIndex + 1.U
      }
      when (dValidSeen === true.B && mem.d.valid === false.B) {
        dValidSeen := false.B
        completed := true.B
        state := Mux(bytesLeft === 0.U, s_idle, s_read)
      }
    }
  }
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
