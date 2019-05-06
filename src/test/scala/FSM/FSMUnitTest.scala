package libpc.FSM

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{Driver, PeekPokeTester, ChiselFlatSpec}

// test sequence 10010
class Seq10010 extends Module {
  val io = IO(new Bundle {
    val input = Input(Bool())
    val output = Output(Bool())
  })

  io.output := false.B

  val fsm = FSM(new FSM {
    entryState("Idle")
      .act {
        io.output := false.B
      }
      .when(io.input === true.B).transferTo("A")

    state("A")
      .when(io.input === true.B).transferTo("B")

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
        io.output := false.B
      }
      .when(io.input === true.B).transferTo("A")
      .otherwise.transferTo("C")
  })
}

class FSMUnitTest_Seq10010(c: Seq10010) extends PeekPokeTester(c) {
  private val N = 100
  private val seq = c
  private val send_v: Seq[Boolean] = (1 to N).map(x => (new scala.util.Random).nextBoolean())

  println(send_v.toString())
  for ((d, id) <- send_v.zipWithIndex)
  {
    poke(seq.io.input, d)
    step(1)
    print(s"cycle $id, output: ")
    print(peek(seq.io.output).toString())
    print("\n")
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
