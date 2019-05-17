package fsm.core

import chisel3._
//import scala.collection.mutable.Stack

object ControlFlowFrontEnd {
  private var name_count = 0
  def get_cnt: Int = {
    val c = name_count
    name_count += 1
    c
  }
}

class ControlFlowFrontEnd extends FSMBase {
  desc = desc + ("_StartState", SkipState())
  // for state record
  var cur_state: String = FSMDescriptionConfig._endStateName

  //
  def equals(that: ControlFlowFrontEnd): Boolean = {
    super.equals(that) && cur_state == that.cur_state
  }
  // for FSM construction
  protected def tick(act: => Unit): StateContext = {
    val state_name = pushState()
    StateContext(state_name).act(act)
  }
  protected def start(act: => Unit): StateContext = {
    val ctxt = tick(act)
    desc = desc.setEntry(cur_state)
    ctxt.copy(is_start = true)
  }
  protected def subFSM(fsm: FSMBase): Unit = {
    val name = super.subFSM(gen_name(), fsm)
    desc = desc +~ UnconditionalTransfer(cur_state, name)
    cur_state = name
  }
  protected def branch(cond: ConditionType)(contents: => Unit): BranchContext = {
    val start_name = pushState(state = SkipState())
    val end_name = insertState(state = SkipState())
    new BranchContext(start_name, end_name).or_branch(cond)(contents)
  }
  protected def run(contents: => Unit): RunContext = {
    val start_name = pushState(state = SkipState())
    contents
    assert(cur_state != start_name, "Must add ticks in run-until loop.")
    val end_name = pushState(state = SkipState())
    new RunContext(start_name, end_name)
  }
  protected def loop(cond: ConditionType)(contents: => Unit): Unit = {
    val start_name = pushState(state = SkipState())
    val end_name = pushState(state = SkipState(), cond = Some(!cond))
    cur_state = start_name
    contents
    assert(cur_state != start_name, "Must add ticks in loop.")
    desc = desc +~ UnconditionalTransfer(cur_state, end_name)
    cur_state = end_name
  }
  protected def repeat(times: Int)(contents: => Unit): Unit = {
    for (_ <- 0 to times)
      contents
  }
  protected def fork(fsms: FSMBase*): String = {
    val name = super.forkFSM(gen_name())(fsms)
    desc = desc +~ UnconditionalTransfer(cur_state, name)
    cur_state = name
    name
  }
  protected def join(fork_name: String): Unit = {
    val join_name = pushState()
    val join_complete = pushState(state = SkipState(),
      cond = Some(desc.findState(fork_name).asInstanceOf[ForkedFSMState].complete_sig))
  }
//  private def repeat(times: UInt)(contents: => Unit): Unit = {
//    val start_name = pushState(state = SkipState())
//    val end_name = insertState(state = SkipState())
//    desc = desc +~ ConditionalTransfer(start_name, end_name, times === 0.U)
//    cur_state = start_name
//    contents
//    if (!desc.findState(cur_state).get.isInstanceOf[GeneralState]) {
//      warn("Last state of repeat is not TikState, a GeneralState is added.")
//      pushState()
//    }
//    desc = desc.procNode(cur_state, x=> x.asInstanceOf[GeneralState].addAct(LastAction(() => {
//      when(counter === times - 1.U) {counter := counter + 1.U} .otherwise {counter := 0.U}
//    })))
//    desc = desc +~ ConditionalTransfer(end_name, start_name, !(counter === times - 1.U))
//    cur_state = end_name
//  }
  protected def goto(dest: String): Unit = {
    desc = desc +~ UnconditionalTransfer(cur_state, dest)
  }
  protected def end: Unit = {
    desc = desc +~ UnconditionalTransfer(cur_state, FSMDescriptionConfig._endStateName)
    cur_state = null
  }
  // help function
  private def insertState(state_name: String = gen_name(),
                           state: NodeType = GeneralState()
                          ): String = {
    desc = desc.insertIfNotFoundG(state_name, state)
    cur_state = state_name
    state_name
  }
  private def pushState(state_name: String = gen_name(),
                           state: NodeType = GeneralState(),
                           cond: Option[ConditionType] = None
                          ): String = {
    val edge =
      if (cond.isEmpty) UnconditionalTransfer(cur_state, state_name)
      else ConditionalTransfer(cur_state, state_name, cond.get)
    insertState(state_name, state)
    desc = desc +~ edge
    cur_state = state_name
    state_name
  }
  protected def gen_name(): String = {
    "_" + ControlFlowFrontEnd.get_cnt.toString
  }

  override def postProc(): Unit = {
    if (cur_state != null) {
      end
    }
  }

  //
  case class StateContext(private var state_name: String, private val is_start: Boolean = false) {
    def act(act: => Unit): this.type = {
      desc = desc.addAct(state_name, () => act)
      this
    }
    def actPre(act: => Unit): this.type = {
      desc = desc.addPre(state_name, () => act)
      this
    }
    def actLast(act: => Unit): this.type = {
      desc = desc.addLast(state_name, () => act)
      this
    }
    def tag(name: String): this.type = {
      desc = desc.renameNode(state_name, name)
      cur_state = name
      state_name = name
      if (is_start) {
        desc = desc.setEntry(name)
      }
      this
    }
  }
  class BranchContext(private val start_name: String, private val end_name: String) {
    protected def new_branch(cond_ : Option[ConditionType], contents: () => Unit): this.type = {
      desc = desc -~ UnconditionalTransfer(start_name, end_name)
      cur_state = start_name
      pushState(state = SkipState(), cond = cond_)
      contents()
      desc = desc +~ UnconditionalTransfer(cur_state, end_name) +~ UnconditionalTransfer(start_name, end_name)
      cur_state = end_name
      this
    }
    def or_branch(cond: ConditionType)(contents: => Unit): this.type = {
      new_branch(Some(cond), () => contents)
    }
    def or(contents: => Unit): Unit = {
      new_branch(None, () => contents)
    }
  }
  class RunContext(val start_name: String, val end_name: String) {
    def until(cond: ConditionType): Unit = {
      desc = desc +~ ConditionalTransfer(end_name, start_name, !cond)
    }
  }
}
