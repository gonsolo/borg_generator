// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._

object ALU {
  val SZ_ALU_FN = 4
}

import ALU._

class AluIo extends Bundle {
  val fn = Input(UInt(SZ_ALU_FN.W))
  val in2 = Input(UInt(32.W))
  val in1 = Input(UInt(32.W))
  val out = Output(UInt(32.W))
}

class ALU extends Module {
   val io = IO(new AluIo)
   io := DontCare
}

