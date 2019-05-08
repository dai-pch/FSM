package libpc.FSM

import chisel3._
//import scala.collection.mutable.Stack

class FSM extends FSMBase {
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
    val state = SubFSMState(fsm)
    desc = desc.insertIfNotFoundG(stateName, state)
    new StateContext(stateName)
  }

  class StateContext(val stateName: String) {
    private def addAct(action: ActionType): Unit = {
      desc = desc.addAct(stateName, action)
    }
    def act(c: => Unit): StateContext = {
      addAct(() => c)
      this
    }
    def when(cond: ConditionType): TransferContext = {
      new TransferContext(this, Some(cond))
    }
    def otherwise: TransferContext = {
      new TransferContext(this, None)
    }

    class TransferContext(val parent: StateContext, val cond: Option[ConditionType]) {
      private val stateName = parent.stateName
      def transferTo(destName: String): StateContext = {
        cond match {
          case Some(c) => desc = desc +~ ConditionalTransfer(stateName, destName, c)
          case None    => desc = desc +~ UnconditionalTransfer(stateName, destName)
        }
        parent
      }
      def transferToEnd: StateContext = {
        cond match {
          case Some(c) => desc = desc +~ ConditionalTransfer(stateName, FSMDescriptionConfig._endStateName, c)
          case None    => desc = desc +~ UnconditionalTransfer(stateName, FSMDescriptionConfig._endStateName)
        }
        parent
      }
    }
  }
  //
}
