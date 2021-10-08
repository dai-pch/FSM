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

class ForkWrapper(private val name: String, private val state: ForkedFSMState) {
  def FSMs: Array[FSMBase] = state.fsms
  val start_sig: Bool = state.start_sig
  val complete_sig: Bool = state.complete_sig
  def state_name: String = this.name
}

class FSMBase(
               // variable
               var desc: FSMDescription = FSMDescription(),
               var default_actions: Array[FSMDescriptionConfig.ActType] = Array(),
               var fork_fsms: Array[FSMBase] = Array(),
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
  protected def forkFSM(stateName: String)(fsms: Seq[FSMBase]): String = {
    val t: Array[FSMBase] = fsms.toArray
    desc = desc.insertIfNotFoundG(stateName, ForkedFSMState(t))
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

object InstantiateFSM {
  def apply(fsm: FSMBase, debug : Boolean = false): FSMBase = {
    val desc = fsm.desc
    val compiler = IdleFSMCompiler(debug = debug)
    val compiled = compiler.compile(fsm)
    Emitter(compiled)
    compiled
  }
}
