package fsm.core

import chisel3._
//import scala.collection.mutable.Stack

class FSMFrontEnd extends FSMBase {
  // for FSM construction
  def state(stateName: String): StateContext = {
    desc = desc.insertIfNotFound(stateName)
    new StateContext(stateName)
  }
  def pseudoState(stateName: String): StateContext = {
    desc = desc.insertIfNotFoundG(stateName, SkipState())
    new StateContext(stateName)
  }
  def entryState(stateName: String): StateContext = {
    if (desc.entryState != FSMDescriptionConfig._endStateName) {
      throw new MultipleEntryException
    }
    desc = desc.insertIfNotFound(stateName)
    desc = desc.setEntry(stateName)
    new StateContext(stateName)
  }
  def subFSM(stateName: String)(fsm: FSMBase): StateContext = {
    super.subFSM(stateName, fsm)
    new StateContext(stateName)
  }
  //
  class StateContext(val state_name: String) {
    def act(c: => Unit): StateContext = {
      desc = desc.addAct(state_name, () => c)
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
