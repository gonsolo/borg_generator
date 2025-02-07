// Copyright Andreas Wendleder 2025
// GPL-3.0-only

package borg

import chisel3._
import chisel3.util.Enum
import freechips.rocketchip.diplomacy.{AddressSet}
import freechips.rocketchip.subsystem.{BaseSubsystem, PBUS}
import freechips.rocketchip.regmapper.{RegField}
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.tilelink.{TLFragmenter, TLRegisterNode}
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

case class BorgConfig()

case object BorgKey extends Field[Option[BorgConfig]](None)

class Borg(beatBytes: Int)(implicit p: Parameters) extends LazyModule {

  val regAddress: BigInt = 0x4000
  val regSize: BigInt = 0x0FFF

  val device = new SimpleDevice("borg-device", Seq("borg,borg-1"))
  val node = TLRegisterNode(Seq(AddressSet(regAddress, regSize)), device, "reg/control", beatBytes=beatBytes)

  lazy val module = new BorgModuleImp(this)
}

class BorgLoaderIO extends Bundle {
  val start = Input(UInt(32.W))
  val counter = Output(UInt(32.W))
}

class BorgLoader extends Module {
  val io = IO(new BorgLoaderIO())

  val start = RegInit(0.U(32.W))
  start := io.start
  when (start === 1.U) {
    start := 0.U
  }
  val counter = RegInit(0.U(32.W))
  io.counter := counter
  when (start === 1.U) {
    counter := counter + 1.U
  }
}

class BorgModuleImp(outer: Borg) extends LazyModuleImp(outer) {

  val config = p(BorgKey).get

  val test1 = RegInit(666.U(32.W))

  val loader = new BorgLoader()

  outer.node.regmap(
    0x00 -> Seq(RegField.r(32, test1)),
    0x20 -> Seq(RegField.w(32, loader.io.start)),
    0x40 -> Seq(RegField.r(32, loader.io.counter)),
  )
}

trait CanHavePeripheryBorg { this: BaseSubsystem =>
  implicit val p: Parameters

  p(BorgKey) .map { k =>
    val pbus = locateTLBusWrapper(PBUS)
    val borg = pbus { LazyModule(new Borg(pbus.beatBytes)(p)) }
    pbus.coupleTo("borg-borg") { borg.node := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
  }
}

class WithBorg() extends Config((site, here, up) => {
  case BorgKey => Some(BorgConfig())
})
