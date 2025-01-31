package borg

import chisel3._
import chisel3.util._
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes, FBUS}
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy.{IdRange}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange}

import org.chipsalliance.diplomacy.lazymodule.{InModuleBody, LazyModule, LazyModuleImp}
import freechips.rocketchip.regmapper.{HasRegMap, RegField}

case class BorgConfig()
case object BorgKey extends Field[Option[BorgConfig]](None)

class Borg(implicit p: Parameters) extends LazyModule {
  val device = new SimpleDevice("borg-device", Seq("borg,borg-1"))

  val regAddress: BigInt = 0x4000
  val beatBytes = 8
  val node = TLRegisterNode(Seq(AddressSet(regAddress, 4096-1)), device, "reg/control", beatBytes=beatBytes)

  val test1 = RegInit(666.U(32.W))
  val test2 = RegInit(1234567.U(32.W))
  node.regmap(
    0x00 -> Seq(RegField.r(32, test1)),
    0x20 -> Seq(RegField.r(32, test2))
  )

  lazy val module = new BorgModuleImp(this)
}

class BorgModuleImp(outer: Borg) extends LazyModuleImp(outer) {
  val config = p(BorgKey).get

}

trait CanHavePeripheryBorg { this: BaseSubsystem =>
  implicit val p: Parameters

  p(BorgKey) .map { k =>
    val fbus = locateTLBusWrapper(FBUS)
    val borg = fbus { LazyModule(new Borg()(p)) }
    //fbus.coupleFrom("init-zero") { _ := initZero.node }
    fbus.coupleTo("borg-borg") { borg.node := TLFragmenter(fbus.beatBytes, fbus.blockBytes) := _ }
  }
}


class WithBorg() extends Config((site, here, up) => {
  case BorgKey => Some(BorgConfig())
})
