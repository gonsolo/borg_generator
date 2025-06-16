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
  TLMonitor,
  TLSlaveParameters,
  TLSlavePortParameters,
  TLXbar
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
  val xbar = TLXbar()
  xbar := borg.core.instructionCache.node
  xbar := borg.core.dataCache.node
  fakeRamNode := xbar

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

  def makeInstructionU(immediate: UInt, rd: UInt, opcode: UInt): UInt = {
    require(immediate.getWidth == 20)
    require(rd.getWidth == 5)
    require(opcode.getWidth == 7)
    Cat(immediate, rd, opcode)
  }

  def makeInstructionI(imm12: UInt, rs1: UInt, funct3: UInt, rd: UInt, opcode: UInt): UInt = {
    require(imm12.getWidth == 12)
    require(rs1.getWidth == 5)
    require(funct3.getWidth == 3)
    require(rd.getWidth == 5)
    require(opcode.getWidth == 7)
    Cat(imm12, rs1, funct3, rd, opcode_addi)
  }

  def makeInstructionF(funct7: UInt, rs2: UInt, rs1: UInt, funct3: UInt, rd: UInt, opcode: UInt): UInt = {
    require(funct7.getWidth == 7)
    require(rs2.getWidth == 5)
    require(rs1.getWidth == 5)
    require(funct3.getWidth == 3)
    require(rd.getWidth == 5)
    require(opcode.getWidth == 7)
    Cat(funct7, rs2, rs1, funct3, rd, opcode_fadd_s)
  }

  def makeInstructionS(immHi: UInt, rs2: UInt, rs1: UInt, funct3: UInt, immLo: UInt, opcode: UInt): UInt = {
    require(immHi.getWidth == 7)
    require(rs2.getWidth == 5)
    require(rs1.getWidth == 5)
    require(funct3.getWidth == 3)
    require(immLo.getWidth == 5)
    require(opcode.getWidth == 7)
    Cat(immHi, rs2, rs1, funct3, immLo, opcode)
  }

  val address = RegNext(io.tl.a.bits.address)

  val instructions = Mem(6, UInt(32.W))

  val load_address = "h5100".U(32.W)

  // Registers
  val x10 = 10.U(5.W) // a0
  val f0 = 0.U(5.W)
  val f1 = 1.U(5.W)
  val f2 = 2.U(5.W)

  // Opcodes
  val opcode_auipc  = "b0010111".U(7.W)
  val opcode_addi   = "b0010011".U(7.W)
  val opcode_flw    = "b0000111".U(7.W)
  val opcode_fadd_s = "b1010011".U(7.W)
  val opcode_fsw    = "b0100111".U(7.W)

  // TODO: val1 and val2 are assumed to be consecutive floats at [a0 + 0] and [a0 + 4]

  // Instruction sequence: Load two floats, add them, store the result.
  // auipc
  // addi
  // flw
  // flw
  // fadd.s
  // fsw

  // auipc a0, %pcrel_hi(symbol)
  {
    val immediate = load_address(31, 12) // upper 20 bits
    val instruction = makeInstructionU(immediate, x10, opcode_auipc)
    instructions(0) := instruction
  }
  // addi a0, a0, %pcrel_lo(symbol)
  {
    val imm12 = load_address(11, 0)
    val funct3 = "b000".U(3.W)
    val rs1 = x10
    val rd = x10
    val instruction = makeInstructionI(imm12, rs1, funct3, rd, opcode_addi)
    instructions(1) := instruction
  }
  // flw f0, 0(a0)
  {
    val imm12 = 0.U(12.W)
    val rs1 = x10
    val funct3 = "b010".U(3.W)
    val rd = f0
    val instruction = makeInstructionI(imm12, rs1, funct3, rd, opcode_flw)
    instructions(2) := instruction
  }
  // flw f1, 4(a0)
  {
    val imm12= 4.U(12.W)
    val rs1 = x10
    val funct3 = "b010".U(3.W)
    val rd = f1
    val instruction = makeInstructionI(imm12, rs1, funct3, rd, opcode_flw)
    instructions(3) := instruction
  }
  // fadd.s f2, f0, f1
  {
    val funct7 = "b0000000".U(7.W)
    val rs2 = f1
    val rs1 = f0
    val funct3 = "b000".U(3.W)
    val rd = f2
    val instruction = makeInstructionF(funct7, rs2, rs1, funct3, rd, opcode_fadd_s)
    instructions(4) := instruction
  }
  // fsw f2, 8(a0)
  {
    val imm = 8.U(12.W)
    val immHi = imm(11, 5)
    val immLo = imm(4, 0)
    val rs2 = f2
    val rs1 = x10
    val funct3 = "b010".U(3.W)
    val instruction = makeInstructionS(immHi, rs2, rs1, funct3, immLo, opcode_fsw)
    instructions(5) := instruction
  }

  def floatToUIntBits(f: Float): BigInt = {
    java.lang.Float.floatToRawIntBits(f) & 0xFFFFFFFFL
  }

  val float0 = 666.6f
  val float1 = 15.3f
  // result: 681.9f
  val dataMemory = Mem(5, UInt(32.W))
  dataMemory.write(0.U, floatToUIntBits(float0).U(32.W))
  dataMemory.write(1.U, floatToUIntBits(float1).U(32.W))

  val a_bits = RegNext(io.tl.a.bits)

  switch(state) {
    is(s_idle) {
      when (io.tl.a.valid) {
        state := s_answer
      }
    }
    is(s_answer) {

      // Fake ram is split into instructions starting at 0x5000 and data starting at 0x5100
      when (address < 0x5100.U) {
        val instruction_index = Wire(UInt(32.W))
        instruction_index := (address - 0x5000.U)/4.U
        val instruction = instructions(instruction_index)
        io.tl.d.bits := edge.AccessAck(a_bits, instruction)
      }.otherwise {
        val data_index = Wire(UInt(32.W))
        data_index := (address - 0x5100.U)/4.U
        val data = dataMemory(data_index)
        printf(cf"FakeRam answer data: 0x$data%x\n")
        io.tl.d.bits := edge.AccessAck(a_bits, data)
      }
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

  val xbar = TLXbar()
  xbar := borg.core.instructionCache.node
  xbar := borg.core.dataCache.node
  fakeRamNode := xbar

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
      tester.clock.step(12)
      //println(tester.reset.asBool.peek().litToBoolean)
      //tester.clock.step(14)
      //tester.io.success.expect(true.B)
    }
  }
}
