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

    cf_run {
      tik {
        io.out := false.B
      }
      tik {
        io.out := true.B
      }
    }.until(io.in)

    cf_while(!io.in) {
      tik {
        io.out := false.B
      }
    }

    cf_if(io.in) {
      tik {

      }
    }
    cf_elif(!io.in) {
      tik {

      }
    }
    cf_else {
      branch("tag1")
    }

  }
}

class FSMExample extends Module {
  val io = IO(new Bundle{
    val input = Input(new Bundle with FSM1Input)
    val output = Output(new Bundle with FSM1Output)
  })

  val fsm = new FSM {
    entryState("Idle") {
      io.output.z_o := false.B
    }.transferTo("s0").inSituation(_.w_i === true.B)

    state("s0") {
      io.output.z_o := false.B
    }
      .transferTo("Idle").inSituation(_.w_i === false.B)
      .transferTo("s1").inSituation(_.w_i === true.B)

    state("s1") {
      out.z_o := true.B
    }
      .transferTo("Idle").inSituation(!_.w_i)
      .transferTo("s1").otherwise

  }
}

object FSMMain extends App {
  iotesters.Driver.execute(args, () => new FSMExample) {
    c => new GCDUnitTester(c)
  }
}
