package libpc.FSM

import chisel3._
//import scala.collection.mutable.Stack

class FSM extends FSMBase {
  type actionType = () => Unit
  type condType = desc.condType

  // for FSM construction
  def state(stateName: String): StateContext = {
    val state = desc.findOrInsert(stateName)
    new StateContext(state)
  }
  def pseudoState(stateName: String): StateContext = {
    val state = desc.findOrInsertG(stateName, new SkipState(stateName))
    println(state)
    new StateContext(state)
  }
  def entryState(stateName: String): StateContext = {
    if (desc.entryState != EndState) {
      throw new MultipleEntryException
    }
    val state = desc.findOrInsert(stateName)
    desc.entryState = state
    new StateContext(state)
  }

  class StateContext(val node: BaseState) {
    private def toTikState(): Option[TikState] = {
      if (!node.isInstanceOf[TikState])
        None
      else
        Some(node.asInstanceOf[TikState])
    }
    private def addAct(action: desc.actionType): Unit = {
      val state = toTikState() match {
        case None => assert(false, "Can not add actions into pseudo state.")
        case Some(s) => s.actionList += action
      }
    }
    def act(c: => Unit): StateContext = {
      addAct(() => c)
      this
    }
    def when(cond: condType): TransferContext = {
      new TransferContext(this, Some(cond))
    }
    def otherwise: TransferContext = {
      new TransferContext(this, None)
    }

    class TransferContext(val parent: StateContext, val cond: Option[condType]) {
      def transferTo(destName: String): StateContext = {
        cond match {
          case Some(c) => desc.conditionalTransferTo(node, desc.findOrHold(destName), c)
          case None =>    desc.unconditionalTransferTo(node, desc.findOrHold(destName))
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
