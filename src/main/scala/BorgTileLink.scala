package borg

import chisel3._
import chisel3.util._

//import freechips.rocketchip.diplomacy.{AddressSet, IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange}
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink.{TLClientNode, TLMasterParameters, TLMasterPortParameters, TLRegisterNode}

import org.chipsalliance.cde.config.{Parameters, Field, Config}
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class BorgTopIO() extends Bundle {
  val borg_busy = Output(Bool())
}

trait HasBorgTopIO {
  def io: BorgTopIO
}

//class BorgTileLink(params: BorgParams, beatBytes: Int)(implicit p: Parameters)
class BorgTileLink(implicit p: Parameters) extends LazyModule {

  val config = p(BorgKey).get

  val device = new SimpleDevice("borg-device", Seq("borg,borg-1"))
  val registerNode = TLRegisterNode(
    Seq(AddressSet(config.regAddress, 4096-1)),
    device,
    "reg/control",
    )
  //val clientNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
  //  name = "dma-test",
  //  sourceId = IdRange(0, 1))))))

  lazy val module = new BorgImpl(this)
  //override lazy val module = new BorgImpl

  //class BorgImpl extends Impl with HasBorgTopIO {
//  class BorgImpl extends LazyModuleImp(this) with HasBorgTopIO {
//    val io = IO(new BorgTopIO)
//    io.borg_busy := false.B
//
//    //withClockAndReset(clock, reset) {
//
//      val (mem, edge) = clientNode.out(0)
//      val addrBits = edge.bundle.addressBits
//      val blockBytes = p(CacheBlockBytes)
//
//      require(params.size % blockBytes == 0)
//
//      val start_loading = RegInit(0.U(1.W))
//
//      val s_init :: s_read :: s_resp :: s_done :: Nil = Enum(4)
//      val state = RegInit(s_init)
//
//      val test1 = RegInit(666.U(32.W))
//      val test2 = RegInit(1234567.U(32.W))
//
//      val addr = Reg(UInt(addrBits.W))
//      val bytesLeft = Reg(UInt(log2Ceil(params.size+1).W))
//
//      mem.a.valid := state === s_read
//      mem.a.bits := edge.Get(
//        fromSource = 0.U,
//        toAddress = addr,
//        lgSize = log2Ceil(blockBytes).U)._2
//
//      mem.d.ready := state === s_resp
//      val dataOut = RegInit(0.U(8.W))
//      dataOut := mem.d.bits.data
//
//      when (state === s_init && start_loading === 1.U) {
//        addr := params.dmaAddress.U
//        bytesLeft := params.size.U
//        state := s_read
//      }
//
//      when (edge.done(mem.a)) {
//        addr := addr + blockBytes.U
//        bytesLeft := bytesLeft - blockBytes.U
//        state := s_resp
//      }
//
//      when (mem.d.fire) {
//        state := Mux(bytesLeft === 0.U, s_done, s_read)
//      }
//
//      when (state === s_done) {
//        start_loading := 0.U
//        //state := s_init
//      }
//
//      registerNode.regmap(
//        0x00 -> Seq(RegField.r(32, test1)),
//        0x20 -> Seq(RegField.r(32, test2))
//        //0x00 -> Seq(RegField.w(1, start_loading)),
//        //0x01 -> Seq(RegField.r(state.getWidth, state))
//      )
//    //}
//  }
}

class BorgImpl(outer: BorgTileLink) extends LazyModuleImp(outer) {
  val test1 = RegInit(666.U(32.W))
  val test2 = RegInit(1234567.U(32.W))
  outer.registerNode.regmap(
        0x00 -> Seq(RegField.r(32, test1)),
        0x20 -> Seq(RegField.r(32, test2))
      )
}
