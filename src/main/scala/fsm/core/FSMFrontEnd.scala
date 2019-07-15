package fsm.core

import chisel3._
//import scala.collection.mutable.Stack

class FSMFrontEnd extends FSMBase {
  // for FSM construction
  protected def state(stateName: String): StateContext = {
    desc = desc.insertIfNotFound(stateName)
    new StateContext(stateName)
  }
  protected def pseudoState(stateName: String): StateContext = {
    desc = desc.insertIfNotFoundG(stateName, SkipState())
    new StateContext(stateName)
  }
  protected def entryState(stateName: String): StateContext = {
    if (desc.entryState != FSMDescriptionConfig._endStateName) {
      throw new MultipleEntryException
    }
    desc = desc.insertIfNotFound(stateName)
    desc = desc.setEntry(stateName)
    new StateContext(stateName)
  }
  protected def subFSM(stateName: String)(fsm: FSMBase): StateContext = {
    super.subFSM(stateName, fsm)
    new StateContext(stateName)
  }
  def forkFSM(stateName: String)(fsms: FSMBase*): ForkContext = {
    val fork_join_name = s"_Fork_${stateName}_Join"
    super.forkFSM(stateName)(fsms)
    state(stateName).otherwise.transferTo(fork_join_name)
    val fork_state = desc.findState(stateName).get.asInstanceOf[ForkedFSMState]
    state(fork_join_name).when(
      !fork_state.complete_sig
    ).transferTo(fork_join_name)
    new ForkContext(fork_join_name, fork_state)
  }
  //
  class StateContext(val state_name: String) {
    def act(c: => Unit): this.type = {
      desc = desc.addAct(state_name, () => c)
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
    def when(cond: ConditionType): TransferContext[this.type] = {
      new TransferContext(this, Some(cond))
    }
    def otherwise: TransferContext[this.type] = {
      new TransferContext(this, None)
    }
    class TransferContext[P <: StateContext](val parent: P, val cond: Option[ConditionType]) {
      private val state_name = parent.state_name
      def transferTo(destName: String): parent.type = {
        cond match {
          case Some(c) => desc = desc +~ ConditionalTransfer(state_name, destName, c)
          case None    => desc = desc +~ UnconditionalTransfer(state_name, destName)
        }
        parent
      }
      def transferToEnd: parent.type = {
        cond match {
          case Some(c) => desc = desc +~ ConditionalTransfer(state_name, FSMDescriptionConfig._endStateName, c)
          case None    => desc = desc +~ UnconditionalTransfer(state_name, FSMDescriptionConfig._endStateName)
        }
        parent
      }
    }
  }
  class ForkContext(state_name: String, val state: ForkedFSMState) extends StateContext(state_name) {
    implicit def toForkWrapper: ForkWrapper = new ForkWrapper(state_name, state)
  }
}
