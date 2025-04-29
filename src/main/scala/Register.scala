// Copyright Andreas Wendleder 2025
// GPL-3.0-only

package borg

import chisel3._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.resources.SimpleDevice
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class Register(address: BigInt)(implicit p: Parameters) extends LazyModule {
  val node = TLRegisterNode(
    address = Seq(AddressSet(address, 0xfff)),
    device = new SimpleDevice("my-reg-device", Seq("myvendor,myreg"))
  )

  lazy val module = new Imp
  class Imp extends LazyModuleImp(this) {
    val myReg = RegInit(0.U(32.W))
    node.regmap(
      0x0 -> Seq(RegField(32, myReg))
    )
  }
}

