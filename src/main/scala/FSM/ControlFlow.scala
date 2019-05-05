package libpc.FSM

import chisel3._
//import scala.collection.mutable.Stack

class ControlFlow extends FSMBase {
  type actionType = () => Unit
  type condType = desc.condType

  // for state record
  var curState: Option[BaseState] = None
  // for FSM construction
  def start(stateName: String): StateContext = {
    if (desc.entryState.nonEmpty) {
      throw new MultipleEntryException
    }
    val state = desc.findOrInsert(stateName)
    desc.entryState = Some(state)
    new StateContext(state)
  }
  def state(stateName: String): StateContext = {
    val state = desc.findOrInsert(stateName)
    new StateContext(state)
  }

  // help function
  private def gen_name(): String = {

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
