// Copyright Andreas Wendleder 2025
// GPL-3.0-only

package borg

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class Driver(edge: TLEdgeOut, address: BigInt) extends Module {
  val io = IO(new Bundle {
    val tl = new TLBundle(edge.bundle)
    val success = Output(Bool())
  })

  val (s_idle :: s_write :: s_write_resp :: s_read :: s_read_resp :: s_done :: Nil) = Enum(6)
  val state = RegInit(s_idle)

  val writeData = 0xdeadbeefL.U(32.W)
  val addr = address.U

  io.tl.a.valid := false.B
  io.tl.a.bits := DontCare
  io.tl.d.ready := true.B
  io.success := false.B

  val d_fired = RegNext(io.tl.d.fire)

  switch(state) {
    is(s_idle) {
      printf(cf"Driver idle\n")
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Put(0.U, addr, 2.U, writeData, 0xf.U)._2
      when(io.tl.a.fire) {
        state := s_write_resp
      }
    }
    is(s_write_resp) {
      when(d_fired && io.tl.d.bits.opcode === TLMessages.AccessAckData) {
        state := s_read
      }
    }
    is(s_read) {
      printf(cf"Driver read\n")
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Get(0.U, addr, 2.U)._2
      when(io.tl.a.fire) { state := s_read_resp }
    }
    is(s_read_resp) {
      printf(cf"Driver read_resp\n")
      when(d_fired) {
        when(io.tl.d.bits.data === writeData) {
          state := s_done
        }
      }
    }
    is(s_done) {
      printf(cf"Driver done\n")
      io.success := true.B
    }
  }
}

