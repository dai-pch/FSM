package fsm.core

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
  type ActType = FSMDescriptionConfig.ActType
  type ConditionType = FSMDescriptionConfig.ConditionType
  // variable
  var desc: FSMDescription = FSMDescription()
  lazy val counter = Reg(UInt())
  val current_state = new FSMStateWrapper
  val next_state = new FSMStateWrapper
  private var counter_width = 0
  // construct function
  protected def subFSM(stateName: String, fsm: FSMBase): String = {
    val state = SubFSMState(fsm)
    desc = desc.insertIfNotFoundG(stateName, state)
    stateName
  }
  // help functions
  def warn(x: String): Unit = {
    Console.err.println("[warning] " + x)
  }
  // helper class
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
