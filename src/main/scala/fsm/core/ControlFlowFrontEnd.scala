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
  // for FSM construction
  def tik(act: => Unit): StateContext = {
    val action: ActType = () => act
    val state_name = pushState(state = GeneralState().addAct(action))
    new StateContext(state_name).act(() => act)
  }
  def start(act: => Unit): StateContext = {
    val ctxt = tik(act)
    desc = desc.setEntry(cur_state)
    ctxt
  }
  def subFSM(fsm: FSMBase): Unit = {
    val name = super.subFSM(gen_name(), fsm)
    desc = desc +~ UnconditionalTransfer(cur_state, name)
    cur_state = name
  }
  def branch(cond: ConditionType)(contents: => Unit): BranchContext = {
    val start_name = pushState(state = SkipState())
    val end_name = insertState(state = SkipState())
    new BranchContext(start_name, end_name).or_branch(cond)(contents)
  }
  def run(contents: => Unit): RunContext = {
    val start_name = pushState(state = SkipState())
    contents
    val end_name = pushState(state = SkipState())
    new RunContext(start_name, end_name)
  }
  def loop(cond: ConditionType)(contents: => Unit): Unit = {
    val start_name = pushState(state = SkipState())
    val end_name = pushState(state = SkipState(), cond = Some(!cond))
    cur_state = start_name
    contents
    desc = desc +~ UnconditionalTransfer(cur_state, end_name)
    cur_state = end_name
  }
  def repeat(times: Int)(contents: => Unit): Unit = {
    for (_ <- 0 to times)
      contents
  }
  protected def repeat(times: UInt)(contents: => Unit): Unit = {
    val start_name = pushState(state = SkipState())
    val end_name = insertState(state = SkipState())
    desc = desc +~ ConditionalTransfer(start_name, end_name, times === 0.U)
    cur_state = start_name
    contents
    if (!desc.findState(cur_state).get.isInstanceOf[GeneralState]) {
      warn("Last state of repeat is not TikState, a GeneralState is added.")
      pushState()
    }
    desc = desc.procNode(cur_state, x=> x.asInstanceOf[GeneralState].addAct(LastAction(() => {
      when(counter === times - 1.U) {counter := counter + 1.U} .otherwise {counter := 0.U}
    })))
    desc = desc +~ ConditionalTransfer(end_name, start_name, !(counter === times - 1.U))
    cur_state = end_name
  }
  def goto(dest: String): Unit = {
    desc = desc +~ UnconditionalTransfer(cur_state, dest)
  }
  // help function
  protected def insertState(state_name: String = gen_name(),
                           state: NodeType = GeneralState()
                          ): String = {
    desc = desc.insertIfNotFoundG(state_name, state)
    cur_state = state_name
    state_name
  }
  protected def pushState(state_name: String = gen_name(),
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
    "_" + ControlFlowFrontEnd.get_cnt.toString()
  }

  //
  class StateContext(val state_name: String) {
    def act(action: ActType): this.type = {
      desc = desc.addAct(state_name, action)
      this
    }
    def tag(name: String): this.type = {
      desc = desc.renameNode(state_name, name)
      this
    }
  }
  class BranchContext(val start_name: String, val end_name: String) {
    protected def new_branch(cond_ : Option[ConditionType], contents: () => Unit): this.type = {
      cur_state = start_name
      pushState(state = SkipState(), cond = cond_)
      contents()
      desc = desc +~ UnconditionalTransfer(cur_state, end_name)
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
      desc = desc +~ ConditionalTransfer(end_name, start_name, cond)
    }
  }
}
