package libpc.FSM

import chisel3._
//import scala.collection.mutable.Stack

class FSM {
  type actionType = () => Unit
  type condType = desc.condType

  val desc = new FSMDescription()
  // for FSM construction
  def state(stateName: String): StateContext = {
    val state = desc.findOrInsert(stateName)
    new StateContext(state)
  }
  def entryState(stateName: String): StateContext = {
    if (desc.entryState.nonEmpty) {
      throw new MultipleEntryException
    }
    val state = desc.findOrInsert(stateName)
    desc.entryState = Some(state)
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
  def apply(fsm: FSM): FSM = {
    val desc = fsm.desc
    val compiler = IdleFSMCompiler
    val compiled = compiler.compile(desc)
    Emitter(compiled)
    fsm
  }
}
