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

class FSMBase(
               // variable
               var desc: FSMDescription = FSMDescription(),
//               lazy val counter: UInt = Reg(UInt()),
               val current_state: FSMStateWrapper = new FSMStateWrapper,
               val next_state: FSMStateWrapper = new FSMStateWrapper,
               private var counter_width: Int = 0
             ) {
  //type info
  type NodeType = FSMDescriptionConfig.NodeType
  type EdgeType = FSMDescriptionConfig.EdgeType
  type ActType = FSMDescriptionConfig.ActType
  type ConditionType = FSMDescriptionConfig.ConditionType
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
  def equals(that: FSMBase): Boolean = {
    desc == that.desc &&
    counter_width == that.counter_width
  }
  def postProc(): Unit = {}
  // helper class
}

object InstanciateFSM {
  def apply(fsm: FSMBase, debug : Boolean = false): FSMBase = {
    val desc = fsm.desc
    val compiler = IdleFSMCompiler(debug = debug)
    val compiled = compiler.compile(fsm)
    Emitter(compiled)
    compiled
  }
}
