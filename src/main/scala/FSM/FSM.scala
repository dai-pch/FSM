package libpc.FSM

import chisel3._

trait FSMPort extends Bundle {}
trait FSMInput extends FSMPort {}
trait FSMOutput extends FSMPort {}

object InstantiateFSM {
  def apply[IT <: FSMInput, OT <: FSMOutput](des: FSMDescription[IT, OT], compiler: FSMCompiler): (IT, OT) = {
    val compiled = compiler.compile(des)
    val module = Module(new FSMTemplate[IT, OT](new Bundle with IT, new Bundle with OT, compiled))
    (module.io.input , module.io.output)
  }
}
