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

  val (mem, edge) = outer.dmaNode.out(0)
  val addressBits = edge.bundle.addressBits
  val dmaBase = 0x88000000L
  val dmaSize = 4 * 0x40L // Write 4 * 64 = 256 bytes
  require(dmaSize % blockBytes == 0)

  val s_init :: s_read :: s_resp :: s_done :: Nil = Enum(4)
  val state = RegInit(s_init)
  val bytesLeft = Reg(UInt(log2Ceil(dmaSize+1).W))
  val data = Reg(UInt(64.W))

  val src = WireInit(0.U)
  val address = Reg(UInt(addressBits.W))
  val size = log2Ceil(blockBytes).U

  val (isLegal, getPutBits) = edge.Get(src, address, size)
  //val (isLegal, getPutBits) = edge.Put(src, address, size, 0.U)
  //printf(cf"Borg state: $state, data: 0x$data%x, isLegal: $isLegal, address: 0x$address%x, size: $size\n")
  //printf(cf"Borg mem.a.ready: ${mem.a.ready}, mem.a.valid: ${mem.a.valid}, mem.d.ready: ${mem.d.ready}, mem.d.valid: ${mem.d.valid}\n")

  mem.a.bits := getPutBits
  data := mem.d.bits.data
  val dValidSeen = RegInit(false.B)

  switch (state) {
    is (s_init) {
      mem.a.valid := false.B
      mem.d.ready := false.B
      when (kick === 1.U) {
        //printf(cf"Borg s_init and kick, address: 0x$address%x, bytesLeft: $bytesLeft, state: $state!\n")
        address := dmaBase.U
        bytesLeft := dmaSize.U
        state := s_read
      }
    }
    is (s_read) {
      mem.a.valid := true.B
      mem.d.ready := false.B
      when (edge.done(mem.a)) {
        //printf(cf"Borg edge done, address 0x$address%x, bytesLeft: $bytesLeft, state: $state!\n")
        address := address + blockBytes.U
        bytesLeft := bytesLeft - blockBytes.U
        state := s_resp
      }
    }
    is (s_resp) {
      mem.a.valid := false.B
      mem.d.ready := true.B
      when (mem.d.valid === true.B) {
        dValidSeen := true.B
        //val hasData = edge.hasData(mem.d.bits)
        //printf(cf"Borg mem.d.valid: data: 0x$data%x, mem.d.bits.data: 0x${mem.d.bits.data}%x, address: 0x$address%x, bytesLeft: $bytesLeft, state: $state, hasData: $hasData.\n")
      }
      when (mem.d.valid === true.B && dValidSeen === false.B) {
        printf(cf"Borg setting data: 0x${mem.d.bits.data}%x.\n")

      }
      when (dValidSeen === true.B && mem.d.valid === false.B) {
        dValidSeen := false.B
        state := Mux(bytesLeft === 0.U, s_done, s_read)
      }
    }
    is (s_done) {
      //printf(cf"Borg s_done.\n")
      mem.a.valid := false.B
      mem.d.ready := false.B
    }
  }
  outer.registerNode.regmap(
    0x00 -> Seq(RegField.r(32, test1)),
    0x20 -> Seq(RegField.r(32, kick)),
    0x40 -> Seq(RegField.w(32, kick)),
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
