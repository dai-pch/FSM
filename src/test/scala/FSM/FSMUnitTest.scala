package libpc.FSM

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{Driver, PeekPokeTester, ChiselFlatSpec}

// test sequence 10010
class Seq10010 extends Module {
  val io = IO(new Bundle {
    val input = Input(Bool())
    val output = Output(Bool())
    val state = Output(UInt())
  })

  io.output := false.B

  val fsm = InstanciateFSM(new FSM {
    entryState("Idle")
      .act {
        io.output := false.B
      }
      .when(io.input === true.B).transferTo("A")

    state("A")
      .when(io.input === false.B).transferTo("B")

    state("B")
      .when(io.input === false.B).transferTo("C")
      .otherwise.transferTo("A")

    state("C")
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

  io.state := fsm.currentState
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

object FSMTester extends App {

  iotesters.Driver.execute(args, () => new Seq10010) {
    c => new FSMUnitTest_Seq10010(c)
  }
}
