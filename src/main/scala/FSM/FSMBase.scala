package libpc.FSM

import chisel3._
//import scala.collection.mutable.Stack

class FSMBase {
  //type info
  type NodeType = FSMDescriptionConfig.NodeType
  type EdgeType = FSMDescriptionConfig.EdgeType
  type ActionType = FSMDescriptionConfig.ActionType
  type ConditionType = FSMDescriptionConfig.ConditionType
  // variable
  var desc: FSMDescription = FSMDescription()
  lazy val currentState: UInt = Wire(UInt())
  lazy val nextState: UInt = Wire(UInt())
}

object InstanciateFSM {
  def apply(fsm: FSMBase): FSMBase = {
    val desc = fsm.desc
    val compiler = IdleFSMCompiler
    val compiled = compiler.compile(desc)
    Emitter(compiled, fsm)
    fsm
  }
}
