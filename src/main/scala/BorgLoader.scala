package borg

import chisel3._

class BorgLoaderIO() extends Bundle {
  val start_loading = Input(UInt(1.W))
}

class BorgLoader() extends Module {
  val io = IO(new BorgLoaderIO())
}

