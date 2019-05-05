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
  val stateName: String = ""
}

abstract class TikState() extends BaseState {
  type actionType = () => Unit
  val actionList = new mutable.ArrayBuffer[actionType]()
}
abstract class PseudoState() extends BaseState {}

case class GeneralState(override val stateName: String) extends TikState {}
//  class StartState() extends BaseState {}
case object EndState extends PseudoState {
  override val stateName = "_EndState"
}

abstract class BaseTransfer extends BaseEdge {
  def source: BaseState
  def destination: BaseState
}
case class ConditionalTransfer(source: BaseState, destination: BaseState, val cond: Bool) extends BaseTransfer {}
case class UnconditionalTransfer(source: BaseState, destination: BaseState) extends BaseTransfer {}

trait Copy[A] {
  def copy: A
}

class FSMDescription() extends DirectedGraph with Cloneable {
  override type NodeType = BaseState
  override type EdgeType = BaseTransfer
  type actionType = () => Unit
  type condType = Bool

  // Graph properties
  val nodeList = new mutable.HashSet[NodeType]()
  var entryState: Option[NodeType] = None
//  val defaultAction = new mutable.ArrayBuffer[actionType]()
  // compile info
  var encode: Map[NodeType, Int] = Map()
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
    val cp = new FSMDescription()
    cp.nodeList ++= nodeList
    cp.entryState = entryState
//    cp.defaultAction ++= defaultAction
    cp.encode = encode
    cp.state_width = state_width
    cp
  }
  def conditionalTransferTo(src: BaseState, dest: BaseState, cond: condType): ConditionalTransfer = {
    val e = ConditionalTransfer(src, dest, cond)
    src.edgeList += e
    e
  }
  def unconditionalTransferTo(src: BaseState, dest: BaseState): UnconditionalTransfer = {
    val e = UnconditionalTransfer(src, dest)
    src.edgeList += e
    e
  }
  def findState(stateName: String): Option[GeneralState] = {
    val search = nodeList.filter(_.stateName == stateName)
    search.headOption
  }
  def findOrInsert(stateName: String): GeneralState = {
    findState(stateName) match {
      case Some(s) => s
      case None    =>
        val s = GeneralState(stateName)
        nodeList += s
        s
    }
  }
}
