package libpc.FSM

import chisel3._
import scala.collection.mutable.Stack

class FSM {
  type actionType = () => Unit
  type condType = desc.condType

  val desc = new FSMDescription()
  // for FSM construction
  def state(stateName: String)(action: actionType): StateContext = {
    val state = desc.findOrInsert(stateName)
    state.actionList += action
    new StateContext(state)
  }
  def entryState(stateName: String)(action: actionType): StateContext = {
    if (desc.entryState.isEmpty) {
      throw new MultipleEntryException
    }
    val state = desc.findOrInsert(stateName)
    desc.entryState = Some(state)
    state.actionList += action
    new StateContext(state)
  }

  class StateContext(val node: GeneralState) {
    def when(cond: condType): TransferContext = {
      new TransferContext(this, Some(cond))
    }
    def otherwise: TransferContext = {
      new TransferContext(this, None)
    }

    class TransferContext(val parent: StateContext, val cond: Option[condType]) {
      def transferTo(destName: String): StateContext = {
        cond match {
          case Some(c) => desc.conditionalTransferTo(node, desc.findOrInsert(destName), c)
          case None =>    desc.unconditionalTransferTo(node, desc.findOrInsert(destName))
        }
        parent
      }
      def transferToEnd: StateContext = {
        cond match {
          case Some(c) => desc.conditionalTransferTo(node, EndState, c)
          case None =>    desc.unconditionalTransferTo(node, EndState)
        }
        parent
      }
    }
  }
  //
}

object FSM {
  def apply(content: () => Unit): FSM = {
    val fsm = new FSM {
      content()
    }
    val desc = fsm.desc
    val compiler = IdleFSMCompiler()
    val compiled = compiler.compile(desc)
    compiled.instantiate()
    fsm
  }
}
