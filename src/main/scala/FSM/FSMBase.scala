package libpc.FSM

import chisel3._
import scala.collection.mutable

class FSMStateWrapper {
  lazy val state_sig = Wire(UInt())
  val state_conds = mutable.Map[String, Bool]()
  def ===(state_name: String): Bool = {
    val res = Wire(Bool())
    state_conds(state_name) = res
    res
  }
  def toUInt: UInt = state_sig
}

object FSMStateWrapper {
  implicit def toUInt(wrapper: FSMStateWrapper): UInt = wrapper.toUInt
}

class FSMBase {
  //type info
  type NodeType = FSMDescriptionConfig.NodeType
  type EdgeType = FSMDescriptionConfig.EdgeType
  type ActionType = FSMDescriptionConfig.ActionType
  type ConditionType = FSMDescriptionConfig.ConditionType
  // variable
  var desc: FSMDescription = FSMDescription()
  val current_state = new FSMStateWrapper
  val next_state = new FSMStateWrapper
}

object InstanciateFSM {
  def apply(fsm: FSMBase, debug_ : Boolean = false): FSMBase = {
    val desc = fsm.desc
    val compiler = IdleFSMCompiler(debug = debug_)
    val compiled = compiler.compile(desc)
    Emitter(compiled, fsm)
    fsm
  }
}
