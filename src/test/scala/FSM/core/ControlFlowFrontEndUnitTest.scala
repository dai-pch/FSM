package fsm.core

import java.io.OutputStream

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}


class PreTest extends Module {
  val io = IO(new Bundle{
    val in = Input(Bool())
    val output = Output(Bool())
    val output_reg = Output(Bool())
    val state = Output(UInt())
  })

  io.output := false.B
  val out_reg = RegInit(false.B)
  io.output_reg := out_reg

  val fsm = InstanciateFSM(new ControlFlowFrontEnd {
    start {
      io.output := false.B
    }.actPre {
      out_reg := false.B
    }.tag("tag2")

    tik {
      io.output := true.B
    }.actPre {
      out_reg := true.B
    }.tag("tag1")

    run {
      tik {
        io.output := false.B
      }.actPre {
        out_reg := false.B
      }.tag("tag3")

      tik {
        io.output := true.B
      }.actPre {
        out_reg := true.B
      }.tag("tag4")
    }.until(io.in)

    loop(!io.in) {
      tik {
        io.output := false.B
      }.actPre {
        out_reg := false.B
      }.tag("loop")
    }

    repeat(0) {
      tik {
        io.output := true.B
      }.actPre {
        out_reg := true.B
      }
    }

    branch(io.in) {
      tik {
        io.output := true.B
      }.actPre {
        out_reg := true.B
      }.tag("br_1")
    }.or_branch(!io.in) {
      tik {
        io.output := false.B
      }.actPre {
        out_reg := false.B
      }.tag("br_2")
    }.or {
      goto("tag1")
    }

    subFSM(new FSMFrontEnd {
      entryState("subStart").act {
        io.output := true.B
      }.actPre {
        out_reg := true.B
      }.otherwise.transferToEnd
    })

    end
  })

  io.state := fsm.current_state
}

class CFUnitTest_PreTest(c: PreTest) extends PeekPokeTester(c) {
  private val N = 5000
  private val d = c
  private val send_v: Seq[Boolean] = (0 until N).map(x => (new scala.util.Random).nextBoolean())

  for (i <- 0 until N)
  {
    poke(d.io.in, send_v(i))
    step(1)
    val comb_o = peek(d.io.output)
    println(comb_o.toString())
    println(peek(d.io.state).toString())
    expect(d.io.output_reg, comb_o)
  }
}

object CFTester extends App {

  iotesters.Driver.execute(args, () => new PreTest) {
    c => new CFUnitTest_PreTest(c)
  }
}
