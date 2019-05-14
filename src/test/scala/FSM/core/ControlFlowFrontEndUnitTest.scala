package fsm.core

import java.io.OutputStream

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}


class CFPreTest extends Module {
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

class CFUnitTest_PreTest(c: CFPreTest) extends PeekPokeTester(c) {
  private val N = 5000
  private val d = c
  private val send_v: Seq[Boolean] = (0 until N).map(x => (new scala.util.Random).nextBoolean())

  for (i <- 0 until N)
  {
    poke(d.io.in, send_v(i))
    step(1)
    val comb_o = peek(d.io.output)
    expect(d.io.output_reg, comb_o)
  }
}

class CFCount21 extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val count = Output(UInt(5.W))
    val done = Output(Bool())
    val state = Output(UInt())
  })

  val cnt = Reg(UInt(5.W))

  io.done := false.B
  io.count := 0.U

  val fsm = InstanciateFSM(new ControlFlowFrontEnd {
    run {
      start {}.tag("start")
    }.until(io.start)

    tik {
      cnt := 0.U
    }.tag("clear")

    run {
      tik {
        cnt := cnt + 1.U
      }.tag("cnt")
    }.until(cnt === 20.U)

    tik {
      io.done := true.B
      io.count := cnt
    }.tag("complete")

    end
  })
  io.state := fsm.current_state
}

class CFUnitTest_Count21(c: CFCount21) extends PeekPokeTester(c) {
  private val N = 50
  private val d = c

//  println(s"Start from state: " + peek(d.io.state).toString())
  poke(d.io.start, false)
  for (i <- 0 until N)
  {
    if (i == 3)
      poke(d.io.start, true)
    else
      poke(d.io.start, false)
    if (i == 26) {
      expect(d.io.done, true)
      expect(d.io.count, 21)
    } else {
      expect(d.io.done, false)
      expect(d.io.count, 0)
    }
//    println("Send " + d.toString())
    step(1)
//    println(s"Cycle ${i+1}, state: " + peek(d.io.state).toString() + ". output: " + peek(d.io.count).toString())
  }
}

class CFClkDiv2 extends Module {
  val io = IO(new Bundle {
    val clk_o = Output(Bool())
    val state = Output(UInt())
  })

  io.clk_o := false.B

  val fsm = InstanciateFSM(new ControlFlowFrontEnd {
    start {
      io.clk_o := false.B
    }

    tik {
      io.clk_o := true.B
    }

    end
  })

  io.state := fsm.current_state
}

class CFUnitTest_ClkDiv2(c: CFClkDiv2) extends PeekPokeTester(c) {
  private val N = 5000
  private val d = c

  //  println(s"Start from state: " + peek(seq.io.state).toString())
  for (i <- 0 until N)
  {
    expect(d.io.clk_o, (i%2) == 1)
    //    println(peek(d.io.state).toString())
    //    println("Send " + d.toString())
    step(1)
    //    println(s"Cycle ${id+1}, state: " + peek(seq.io.state).toString() + ". output: " + peek(seq.io.output).toString())
  }
}


object CFTester extends App {

  iotesters.Driver.execute(args, () => new CFPreTest) {
    c => new CFUnitTest_PreTest(c)
  }
  iotesters.Driver.execute(args, () => new CFCount21) {
    c => new CFUnitTest_Count21(c)
  }
  iotesters.Driver.execute(args, () => new CFClkDiv2) {
    c => new CFUnitTest_ClkDiv2(c)
  }
}
