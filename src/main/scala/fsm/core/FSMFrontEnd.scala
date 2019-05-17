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
  def forkFSM(stateName: String)(fsms: FSMBase*): StateContext = {
    super.forkFSM(stateName)(fsms)
    new StateContext(stateName)
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
    def when(cond: ConditionType): TransferContext = {
      new TransferContext(this, Some(cond))
    }
    def otherwise: TransferContext = {
      new TransferContext(this, None)
    }

    class TransferContext(val parent: StateContext, val cond: Option[ConditionType]) {
      private val state_name = parent.state_name
      def transferTo(destName: String): StateContext = {
        cond match {
          case Some(c) => desc = desc +~ ConditionalTransfer(state_name, destName, c)
          case None    => desc = desc +~ UnconditionalTransfer(state_name, destName)
        }
        parent
      }
      def transferToEnd: StateContext = {
        cond match {
          case Some(c) => desc = desc +~ ConditionalTransfer(state_name, FSMDescriptionConfig._endStateName, c)
          case None    => desc = desc +~ UnconditionalTransfer(state_name, FSMDescriptionConfig._endStateName)
        }
        parent
      }
    }
  }
}
