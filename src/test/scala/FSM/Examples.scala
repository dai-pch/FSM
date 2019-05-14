import fsm._
import chisel3._

class CFExample extends Module {
  val io = IO(new Bundle{
    val in = Input(Bool())
    val output = Output(Bool())
  })

  io.output := false.B

  val fsm = InstanciateFSM(new ControlFlow {
    start {
      io.output := false.B
    }

    tick {
      io.output := true.B
    }.tag("tag1")

    run {
      tick {
        io.output := false.B
      }

      tick {
        io.output := true.B
      }
    }.until(io.in)

    loop(!io.in) {
      tick {
        io.output := false.B
      }
    }

    repeat(3) {
      tick {
        io.output := true.B
      }
    }

    branch(io.in) {
      tick {

      }
    }.or_branch(!io.in) {
      tick {

      }
    }.or {
      goto("tag1")
    }

    subFSM(new FSM {
      entryState("subStart")
        .otherwise.transferToEnd
    })

    end

  })
}

class FSMExample extends Module {
  val io = IO(new Bundle {
    val w_i = Input(Bool())
    val z_o = Output(Bool())
  })

  io.z_o := false.B

  val fsm = InstanciateFSM(new FSM {
    entryState("Idle")
      .act {
        io.z_o := false.B
      }
      .when(io.w_i === true.B).transferTo("s0")

    state("s0")
      .act {
        io.z_o := false.B
      }
      .when(io.w_i === false.B).transferTo("Idle")
      .when(io.w_i === true.B).transferTo("s1")

    state("s1")
      .act {
        io.z_o := true.B
      }
      .when(!io.w_i).transferTo("Idle")
      .otherwise.transferTo("s1")
  })
}

object GenExamples extends App {
  chisel3.Driver.execute(args, () => new FSMExample)
  chisel3.Driver.execute(args, () => new CFExample)
}
