// Copyright Andreas Wendleder 2025
// CERN-OHL-S-2.0

package borg

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import chisel3.util.{Cat, Enum, is, switch}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, TransferSizes}
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink.{
  TLBundle,
  TLClientNode,
  TLEdgeIn,
  TLEdgeOut,
  TLFragmenter,
  TLManagerNode,
  TLManagerParameters,
  TLMasterParameters,
  TLMasterPortParameters,
  TLMessages,
  TLSlaveParameters,
  TLSlavePortParameters
}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class BorgFirstDriver(edge: TLEdgeOut, address: BigInt) extends Module {
  val io = IO(new Bundle {
    val tl = new TLBundle(edge.bundle)
    val success = Output(Bool())
  })

  val (s_read :: s_read_resp :: s_done :: Nil) = Enum(3)
  val state = RegInit(s_read)

  val readData = 666.U(32.W)
  val addr = address.U

  io.tl.a.valid := false.B
  io.tl.a.bits := DontCare
  io.tl.d.ready := true.B
  io.success := false.B

  val d_fired = RegNext(io.tl.d.fire)

  switch(state) {
    is(s_read) {
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Get(0.U, addr, 2.U)._2
      when(io.tl.a.fire) { state := s_read_resp }
    }
    is(s_read_resp) {
      when(d_fired) {
        when(io.tl.d.bits.data === readData) {
          state := s_done
        }
      }
    }
    is(s_done) {
      io.success := true.B
    }
  }
}

class BorgFirstHarness(implicit p: Parameters) extends LazyModule {
  val borg = LazyModule(new Borg(8))
  val registerDriverNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "borgDriver")))
    )
  )

  borg.registerNode := TLFragmenter(8, 64) := registerDriverNode

  val fakeRamNode = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address = Seq(AddressSet(0x5000, 0xfff)),
            supportsGet = TransferSizes(1, 8),
            supportsPutFull = TransferSizes(1, 8),
            fifoId = Some(0)
          )
        ),
        beatBytes = 8
      )
    )
  )

  fakeRamNode := TLFragmenter(8, 64) := borg.core.instructionCache.node

  lazy val module = Module(new Imp)
  class Imp extends LazyModuleImp(this) {
    val (registerDriverOut, registerDriverEdge) = registerDriverNode.out(0)
    val registerDriver= Module(new BorgFirstDriver(registerDriverEdge, 0x4000))
    registerDriverOut.a <> registerDriver.io.tl.a
    registerDriverOut.d <> registerDriver.io.tl.d

    val (fakeRamIn, fakeRamEdge) = fakeRamNode.in(0)
    val fakeRam = Module(new FakeRam(fakeRamEdge))
    fakeRamIn.a <> fakeRam.io.tl.a
    fakeRamIn.d <> fakeRam.io.tl.d

    borg.module.reset := reset

    val io = IO(new BorgIO)
    io.success := registerDriver.io.success
  }
}

class BorgIO extends Bundle {
  val success = Output(Bool())
}

class BorgFirstTester(implicit p: Parameters) extends Module {
  val harness = LazyModule(new BorgFirstHarness())
  val io = IO(new BorgIO)
  io.success := harness.module.io.success
}

class BorgFirstTest extends AnyFlatSpec {
  behavior of "Borg"
  it should "do something" in {
    implicit val p: Parameters = Parameters.empty
    simulate(new BorgFirstTester()) { tester =>
      tester.reset.poke(true.B)
      tester.clock.step()
      tester.reset.poke(false.B)
      tester.clock.step(2)
      tester.io.success.expect(true.B)
    }
  }
}

class BorgRegisterDriver(edge: TLEdgeOut) extends Module {
  val io = IO(new Bundle {
    val tl = new TLBundle(edge.bundle)
    val success = Output(Bool())
  })

  val (s_write :: s_write_resp :: s_read :: s_read_resp :: s_done :: Nil) =
    Enum(5)
  val state = RegInit(s_write)

  val writeData = 1.U(32.W)
  val readData = 1.U(32.W)
  val kickAddress = 0x4020.U
  val completedAddress = 0x4040.U

  io.tl.a.valid := false.B
  io.tl.a.bits := DontCare
  io.tl.d.ready := true.B
  io.success := false.B

  val d_fired = RegNext(io.tl.d.fire)

  switch(state) {
    is(s_write) {
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Put(0.U, kickAddress, 2.U, writeData, 0xf.U)._2
      when(io.tl.a.fire) { state := s_write_resp }
    }
    is(s_write_resp) {
      when(d_fired && io.tl.d.bits.opcode === TLMessages.AccessAckData) {
        state := s_read
      }
    }
    is(s_read) {
      io.tl.a.valid := true.B
      io.tl.a.bits := edge.Get(0.U, completedAddress, 2.U)._2
      when(io.tl.a.fire) { state := s_read_resp }
    }
    is(s_read_resp) {
      //printf(cf"  data: ${io.tl.d.bits.data}, expected: $readData\n")
      when(d_fired && io.tl.d.bits.data =/= readData) {
        state := s_read
      }
      when(d_fired && io.tl.d.bits.data === readData) {
        state := s_done
      }
    }
    is(s_done) {
      io.success := true.B
    }
  }
}

class FakeRam(edge: TLEdgeIn) extends Module {
  val io = IO(new Bundle {
    val tl = Flipped(new TLBundle(edge.bundle))
    val success = Output(Bool())
  })
  io.success := DontCare
  io.tl.b := DontCare
  io.tl.c := DontCare
  io.tl.d := DontCare
  io.tl.e := DontCare

  val (s_idle :: s_answer :: Nil) = Enum(2)
  val state = RegInit(s_idle)

  io.tl.a.ready := state === s_idle
  io.tl.d.valid := state === s_answer

  val address = RegNext(io.tl.a.bits.address)

  val instructions = Mem(5, UInt(32.W))

  val load_address = "h5100".U(32.W)

  // Load upper from address 0x5100 into a0 using lui
  {
    val immediate = load_address(31, 12)
    val rd = 10.U(5.W) // a0 = register 10
    val opcode = "b0110111".U(7.W)
    val instruction = Cat(immediate, rd, opcode) // U format
    instructions(0) := instruction
  }
   // Load rest of address 0x5100 into a0 using addi
  {
    val immediate = load_address(11, 0)
    val rs1 = 0.U(5.W)
    val funct3 = "h0".U(3.W)
    val rd = 10.U(5.W) // a0 = register 10
    val opcode = "b0010011".U(7.W)
    val instruction = Cat(immediate, rs1, funct3, rd, opcode) // I format
    instructions(1) := instruction
  }
  // Load value 1 from address to a1 with lw
  {
    val immediate = 0.U(12.W)
    val rs1 = 10.U(5.W) // register 10 from above
    val funct3 = "h2".U(3.W)
    val rd = 5.U(5.W) // Load to t0 = register 5
    val opcode = "b0000011".U(7.W)
    val instruction = Cat(immediate, rs1, funct3, rd, opcode) // I format
    instructions(2) := instruction
  }
  // Add 666 to register t0 with addi
  {
    val immediate = 666.U(12.W)
    val rs1 = 5.U(5.W) // Add to t0 = register 5
    val funct3 = "h0".U(3.W)
    val rd = 5.U(5.W) // Store to t0 again
    val opcode = "b0010011".U(7.W)
    val instruction = Cat(immediate, rs1, funct3, rd, opcode) // I format
    instructions(3) := instruction
  }
  // Store value with sw
  {
    val immediate = 0.U(12.W)
    val rs2 = 5.U(5.W) // Get value from t0 = register 5
    val rs1 = 10.U(5.W) // Get store address from register a0 = 10
    val funct3 = "h2".U(3.W)
    val opcode = "b0100011".U(7.W)
    val instruction = Cat(immediate(11, 5), rs2, rs1, funct3, immediate(4, 0), opcode) // S format
    instructions(4) := instruction
  }

  //printf(cf"FakeRam state: $state\n")
  switch(state) {
    is(s_idle) {
      //printf(cf"FakeRam idle, a valid: ${io.tl.a.valid} a address: 0x${io.tl.a.bits.address}%x\n")
      when (io.tl.a.valid) {
        state := s_answer
      }
    }
    is(s_answer) {
      val instruction_index = Wire(UInt(32.W))
      instruction_index := (address - 0x5000.U)/4.U
      val instruction = instructions(instruction_index)
      io.tl.d.bits := edge.AccessAck(io.tl.a.bits, instruction)
      printf(cf"FakeRam answer, address: 0x$address%x, instruction_index: $instruction_index, instruction: $instruction%b\n")

      state := s_idle
    }
  }
}

class BorgKickHarness(implicit p: Parameters) extends LazyModule {
  val borg = LazyModule(new Borg(8))
  val registerDriverNode = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "borgDriver")))
    )
  )

  borg.registerNode := TLFragmenter(8, 64) := registerDriverNode

  val fakeRamNode = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address = Seq(AddressSet(0x5000, 0xfff)),
            supportsGet = TransferSizes(1, 8),
            supportsPutFull = TransferSizes(1, 8),
            fifoId = Some(0)
          )
        ),
        beatBytes = 8
      )
    )
  )

  fakeRamNode := TLFragmenter(8, 64) := borg.core.instructionCache.node

  lazy val module = Module(new Imp)
  class Imp extends LazyModuleImp(this) {
    val (registerDriverOut, registerDriverEdge) = registerDriverNode.out(0)
    val registerDriver = Module(new BorgRegisterDriver(registerDriverEdge))
    registerDriverOut.a <> registerDriver.io.tl.a
    registerDriverOut.d <> registerDriver.io.tl.d

    val (fakeRamIn, fakeRamEdge) = fakeRamNode.in(0)
    val fakeRam = Module(new FakeRam(fakeRamEdge))
    fakeRamIn.a <> fakeRam.io.tl.a
    fakeRamIn.d <> fakeRam.io.tl.d

    borg.module.reset := reset

    val io = IO(new BorgIO)
    io.success := registerDriver.io.success
  }
}

class BorgKickTester(implicit p: Parameters) extends Module {
  val harness = LazyModule(new BorgKickHarness())

  harness.module.reset := reset

  val io = IO(new BorgIO)
  io.success := harness.module.io.success
}

class BorgKickTest extends AnyFlatSpec {
  behavior of "Borg"
  it should "do something" in {
    implicit val p: Parameters = Parameters.empty
    simulate(new BorgKickTester()) { tester =>
      tester.reset.poke(true.B)
      tester.clock.step()
      tester.reset.poke(false.B)
      tester.clock.step(16)
      //println(tester.reset.asBool.peek().litToBoolean)
      //tester.clock.step(14)
      //tester.io.success.expect(true.B)
    }
  }
}
