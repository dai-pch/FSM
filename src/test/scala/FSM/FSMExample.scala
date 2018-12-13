package libpc.FSM

import chisel3._

trait FSM1Input extends FSMInput {
  val w_i = Bool()
}

trait FSM1Output extends FSMOutput {
  val z_o = Bool()
}

class FSMExample extends Module {
  val io = IO(new Bundle{
    val input = Input(new Bundle with FSM1Input)
    val output = Output(new Bundle with FSM1Output)
  })

  val (fsmin, fsmout) = InstantiateFSM(new Fsm1, IdleFSMCompile)
  fsmin <> io.input
  fsmout <> io.output
}

class Fsm1 extends FSMDescription[FSM1Input, FSM1Output] {
  entryState("Idle")
    .output((in, out) => out.z_o := false.B)
    .transferTo("s0").inSituation(_.w_i === true.B)

  state("s0")
    .output((in, out) => out.z_o := false.B)
    .transferTo("Idle").inSituation(_.w_i === false.B)
    .transferTo("s1").inSituation(_.w_i === true.B)

  state("s1")
    .output((in, out) => out.z_o := true.B)
    .transferTo("Idle").inSituation(!_.w_i)
    .transferTo("s1").otherwise
}

object FSMMain extends App {
  iotesters.Driver.execute(args, () => new FSMExample) {
    c => new GCDUnitTester(c)
  }
}
