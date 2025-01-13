package borg

import chisel3._

import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink.TLRegisterNode

class BorgLoaderIO() extends Bundle {
  val start_loading = Input(UInt(1.W))
}

class BorgLoader() extends Module {
  val io = IO(new BorgLoaderIO())
}

