package fsm.core

import java.io.OutputStream

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

// test sequence 10010
class Seq10010 extends Module {
  val io = IO(new Bundle {
    val input = Input(Bool())
    val output = Output(Bool())
    val state = Output(UInt())
  })

  io.output := false.B

  val fsm = InstanciateFSM(new FSMFrontEnd {
    entryState("Idle")
      .act {
        io.output := false.B
      }
      .when(io.input === true.B).transferTo("A")

    state("A")
      .when(io.input === false.B).transferTo("B")

    state("B")
      .when(io.input === false.B).transferTo("C")
      .otherwise.transferTo("pseB")

    pseudoState("pseB")
      .otherwise.transferTo("A")

    state("C")
      .otherwise.transferTo("pseC")

    pseudoState("pseC")
      .when(io.input === true.B).transferTo("D")
      .otherwise.transferTo("Idle")

    state("D")
      .when(io.input === true.B).transferTo("A")
      .otherwise.transferTo("E")

    state("E")
      .act {
        io.output := true.B
      }
      .when(io.input === true.B).transferTo("A")
      .otherwise.transferTo("C")
  })

  io.state := fsm.current_state
}

class FSMUnitTest_Seq10010(c: Seq10010) extends PeekPokeTester(c) {
  private val N = 50000
  private val seq = c
  private val send_v: Seq[Boolean] = (1 to N).map(x => (new scala.util.Random).nextBoolean())

//  println(s"Start from state: " + peek(seq.io.state).toString())
  for ((d, id) <- send_v.zipWithIndex)
  {
    poke(seq.io.input, d)
//    println("Send " + d.toString())
    step(1)
//    println(s"Cycle ${id+1}, state: " + peek(seq.io.state).toString() + ". output: " + peek(seq.io.output).toString())
    if (id >= 4 && send_v.slice(id-4, id+1) == Seq(true, false, false, true, false))
      expect(seq.io.output, true)
    else
      expect(seq.io.output, false)
  }
}

class Count21 extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val count = Output(UInt(5.W))
    val done = Output(Bool())
  })

  val cnt = Reg(UInt(5.W))

  io.done := false.B
  io.count := 0.U

  val fsm = InstanciateFSM(new FSMFrontEnd {
    entryState("Idle")
      .when(io.start).transferTo("ClearCnt")

    state("ClearCnt")
      .act {
        cnt := 0.U
      }
      .otherwise.transferTo("Run")

    subFSM("Run")(new FSMFrontEnd {
      entryState("Add")
        .act {
          cnt := cnt + 1.U
        }
        .when(cnt === 20.U).transferToEnd
        .otherwise.transferTo("Add")
    })
      .otherwise.transferTo("Done")

    state("Done")
      .act {
        io.done := true.B
        io.count := cnt
      }
      .otherwise.transferToEnd
  })
}

class FSMUnitTest_Count21(c: Count21) extends PeekPokeTester(c) {
  private val N = 50
  private val count = c

  //  println(s"Start from state: " + peek(seq.io.state).toString())
  poke(count.io.start, false)
  for (i <- 0 to 25)
  {
    if (i == 3)
      poke(count.io.start, true)
    else
      poke(count.io.start, false)
    if (i == 26) {
      expect(count.io.done, true)
      expect(count.io.count, 21)
    } else {
      expect(count.io.done, false)
      expect(count.io.count, 0)
    }
    //    println("Send " + d.toString())
    step(1)
    //    println(s"Cycle ${id+1}, state: " + peek(seq.io.state).toString() + ". output: " + peek(seq.io.output).toString())
  }
}

class ClkDiv2 extends Module {
  val io = IO(new Bundle {
    val clk_o = Output(Bool())
    val state = Output(UInt())
  })

  io.clk_o := false.B

  val fsm = InstanciateFSM(new FSMFrontEnd {
    entryState("Zero")
      .act {
        io.clk_o := false.B
      }
      .otherwise.transferTo("One")

    state("One")
      .act {
        io.clk_o := true.B
      }
      .otherwise.transferTo("Zero")
  })

  io.state := fsm.current_state
}

class FSMUnitTest_ClkDiv2(c: ClkDiv2) extends PeekPokeTester(c) {
  private val N = 5000
  private val d = c

  //  println(s"Start from state: " + peek(seq.io.state).toString())
  for (i <- 0 to N)
  {
    expect(d.io.clk_o, (i%2) == 1)
//    println(peek(d.io.state).toString())
    //    println("Send " + d.toString())
    step(1)
    //    println(s"Cycle ${id+1}, state: " + peek(seq.io.state).toString() + ". output: " + peek(seq.io.output).toString())
  }
}

object FSMTester extends App {

  iotesters.Driver.execute(args, () => new Seq10010) {
    c => new FSMUnitTest_Seq10010(c)
  }
  iotesters.Driver.execute(args, () => new Count21) {
    c => new FSMUnitTest_Count21(c)
  }
  iotesters.Driver.execute(args, () => new ClkDiv2) {
    c => new FSMUnitTest_ClkDiv2(c)
  }
}
