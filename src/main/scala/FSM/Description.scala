package libpc.FSM

import scala.collection.mutable
import chisel3._


abstract class BaseNode {
  var edgeList: Seq[BaseEdge]
}

abstract class BaseEdge {
  var source: BaseNode
  var destination: BaseNode
}

abstract class DirectedGraph[KeyType] {
  type NodeType <: BaseNode
  type EdgeType <: BaseEdge
  val nodeList: mutable.ArrayBuffer[NodeType]
}


abstract class BaseState() extends BaseNode {
  override var edgeList = new mutable.ArrayBuffer[BaseTransfer]()
  val stateName: String
}

case class GeneralState[-InputType <: Data, -OutputType <: Data](stateName: String) extends BaseState{
  type condType = InputType => Bool
  type actionType = (InputType, OutputType) => Unit
  val actionList = new mutable.ArrayBuffer[actionType]()
}
//  class StartState() extends BaseState {}
case object EndState extends BaseState {
  val stateName = "_EndState"
}

abstract class BaseTransfer() extends BaseEdge {}
case class ConditionalTransfer[-InputType <: Data](source: BaseState, destination: BaseState, cond: (InputType => Bool) = ((_:InputType) => (true.B))) extends BaseTransfer {}
case class UnconditionalTransfer(source: BaseState, destination: BaseState) extends BaseTransfer {}

trait Copy[A] {
  def copy: A
}

class FSMDescription[InputType <: Data, OutputType <: Data]() extends DirectedGraph with Cloneable {
  type NodeType = BaseState
  type GeneralNode = GeneralState[InputType, OutputType]
  type EdgeType = BaseTransfer
  type condType = InputType => Bool
  type actionType = (InputType, OutputType) => Unit

  // Graph properties
  val nodeList = new mutable.HashSet[GeneralNode]()
  var entryState: Option[NodeType] = None
  val defaultAction = new mutable.ArrayBuffer[actionType]()
  // compile info
  var encode: Map[BaseState, Int] = Map()
  var state_width: Int = 0
  // class methods
  def traverseNode(func: (NodeType => Unit)): Unit = {
    for (node <- nodeList) {
      func(node)
    }
  }
  def traverseEdge(func: (EdgeType => Unit)): Unit = {
    traverseNode((node: NodeType) => node.edgeList.map(func(_)))
  }
  def copy() = {
    val cp = new FSMDescription[InputType, OutputType]()
    cp.nodeList ++= nodeList
    cp.entryState = entryState
    cp.defaultAction ++= defaultAction
    cp.encode = encode
    cp.state_width = state_width
    cp
  }
  def conditionalTransferTo(src: BaseState, dest: BaseState, cond: condType): ConditionalTransfer[InputType] = {
    val e = ConditionalTransfer(src, dest, cond)
    src.edgeList += e
    e
  }
  def unconditionalTransferTo(src: BaseState, dest: BaseState): UnconditionalTransfer = {
    val e = UnconditionalTransfer(src, dest)
    src.edgeList += e
    e
  }
  def findState(stateName: String): Option[GeneralNode] = {
    val search = nodeList.filter(_.stateName == stateName)
    search.headOption
  }
  def findOrInsert(stateName: String): GeneralNode = {
    findState(stateName) match {
      case Some(s) => s
      case None    =>
        val s = GeneralState(stateName)
        nodeList += s
        s
    }
  }
  // for FSM construction
  def state(stateName: String): StateContext = {
    val state = findOrInsert(stateName)
    new StateContext(state)
  }
  def entryState(stateName: String): StateContext = {
    if (entryState.isEmpty) {
      throw new MultipleEntryException
    }
    val state = findOrInsert(stateName)
    entryState = Some(state)
    new StateContext(state)
  }

  class StateContext(val node: GeneralNode) {
    def transferTo(destName: String): TransferContext = {
      new TransferContext(this, findOrInsert(destName))
    }
    def transferToEnd: TransferContext = {
      new TransferContext(this, EndState)
    }
    def output(action: actionType): StateContext = {
      node.actionList += action
      this
    }

    class TransferContext(val parent: StateContext, val dest: BaseState) {
      def inSituation(cond: condType): StateContext = {
        conditionalTransferTo(node, dest, cond)
        parent
      }
      def otherwise: StateContext = {
        unconditionalTransferTo(node, dest)
        parent
      }
    }
  }
  //
}
