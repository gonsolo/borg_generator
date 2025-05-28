// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.util._

trait RISCVConstants {
  // The Bubble Instruction (Machine generated NOP)
  val BUBBLE = 0x4033.U(32.W)

  // The most and least significant bits of the destination register rd
  val RD_MSB = 11
  val RD_LSB = 7

  // The most and least significant bits of the first source register rs1
  val RS1_MSB = 19
  val RS1_LSB = 15

  val RS2_MSB = 24
  val RS2_LSB = 20
}

trait MemoryOpConstants {
  val DPORT = 0
  val IPORT = 1

  val MEMORY_X      =  0.U(1.W)
  val MEMORY_READ   =  0.U(1.W)
  val MEMORY_WRITE  =  1.U(1.W)

  val MT_X          =  0.U(3.W)
}

trait ScalarOpConstants
{
  // RS1 Operand Select Signal
  val OP1_X     =  0.U(2.W)
  val OP1_RS1   =  1.U(2.W)
  val OP1_IMU   =  2.U(2.W)

  val OP2_IMI   =  1.U(2.W) // immediate, I-type 
  val OP2_IMS   =  2.U(2.W) // immediate, S-type

  // ALU Operation Signal
  val ALU_X     =  0.U(4.W) // unspecified alu function
  val ALU_ADD   =  1.U(4.W) // add alu function
  val ALU_COPY1 = 11.U(4.W)

  // Writeback Select Signal
  val WB_X      =  0.U(2.W)
  val WB_ALU    =  1.U(2.W)
  val WB_MEM    =  2.U(2.W)

  // Memory Enable Signal
  val MEMORY_ENABLE     = true.B
  val MEMORY_DISABLE    = false.B

  // Register File Write Enable Signal
  val REN_0     = false.B
  val REN_1     = true.B

  // Is the instruction a load
  val LOAD_0    = false.B
  val LOAD_1    = true.B
}

object Constants extends RISCVConstants with MemoryOpConstants with ScalarOpConstants {}

import Constants._

class MemoryRequest() extends Bundle {
  val address = Output(UInt(32.W))
  val function = Output(UInt(MEMORY_READ.getWidth.W))
  val data = Output(UInt(32.W))

  val typ  = Output(UInt(MT_X.getWidth.W)) // memory type
}

class MemoryResponse() extends Bundle {
  val data = Output(UInt(32.W))
}

class MemoryPortIo() extends Bundle {
  val request = Flipped(new DecoupledIO(new MemoryRequest()))
  val response = new ValidIO(new MemoryResponse())
}

