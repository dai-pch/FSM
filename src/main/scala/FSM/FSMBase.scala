package libpc.FSM

import chisel3._
//import scala.collection.mutable.Stack

class FSMBase {
  val desc = new FSMDescription()
}

object FSM {
  def apply(fsm: FSMBase): FSMBase = {
    val desc = fsm.desc
    val compiler = IdleFSMCompiler
    val compiled = compiler.compile(desc)
    Emitter(compiled)
    fsm
  }
}
