package libpc.FSM

import chisel3._

trait FSM1Input extends Bundle {
  val w_i = Bool()
}

trait FSM1Output extends Bundle {
  val z_o = Bool()
}

class FSMExample extends Module {
  val io = IO(new Bundle {
    val input = Input(new Bundle with FSM1Input)
    val output = Output(new Bundle with FSM1Output)
  })

  io.output.z_o := false.B

  val fsm = InstanciateFSM(new FSM {
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
  chisel3.Driver.execute(args, () => new FSMExample)
}
