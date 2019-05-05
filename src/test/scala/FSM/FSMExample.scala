package libpc.FSM

import chisel3._

trait FSM1Input extends FSMInput {
  val w_i = Bool()
}

trait FSM1Output extends FSMOutput {
  val z_o = Bool()
}

class CFExample extends Module {
  val io = IO(new Bundle{
    val in = Input(Bool())
    val output = Output(Bool())
  })

  val fsm = new ControlFlow {
    start(reset) {
      io.out := false.B
    }

    tik {
      io.out := true.B
    }.tag("tag1")

    run {
      tik {
        io.out := false.B
      }
      tik {
        io.out := true.B
      }
    }.until(io.in)

    loop(!io.in) {
      tik {
        io.out := false.B
      }
    }

    branch(io.in) {
      tik {

      }
    } or_branch(!io.in) {
      tik {

      }
    } or {
      goto("tag1")
    }

  }
}

class FSMExample extends Module {
  val io = IO(new Bundle{
    val input = Input(new Bundle with FSM1Input)
    val output = Output(new Bundle with FSM1Output)
  })

  val fsm = FSM(new FSM {
    entryState("Idle")
      .act {
        io.output.z_o := false.B
      }
      .when(io.input.w_i === true.B).transferTo("s0")

    state("s0")
      .act {
        io.output.z_o := false.B
      }
      .when(io.input.w_i === false.B).transferTo("Idle")
      .when(io.input.w_i === true.B).transferTo("s1")

    state("s1")
      .act {
        io.output.z_o := true.B
      }
      .when(!io.input.w_i).transferTo("Idle")
      .otherwise.transferTo("s1")
  })
}

object FSMMain extends App {
  chisel3.Driver.execute(args, () => new FSMExample) {}
}
