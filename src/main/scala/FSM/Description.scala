package libpc.FSM

import scala.collection.mutable
import chisel3._


sealed abstract class BaseState {
  var edgeList = new mutable.ArrayBuffer[BaseTransfer]()
  val stateName: String
}

sealed abstract class TikState extends BaseState {
  type actionType = () => Unit
  val actionList = new mutable.ArrayBuffer[actionType]()
}
sealed abstract class PseudoState() extends BaseState {}

sealed case class GeneralState(override val stateName: String) extends TikState {}
//  class StartState() extends BaseState {}
case object EndState extends PseudoState {
  override val stateName = "_EndState"
}
sealed case class SkipState(override val stateName: String) extends PseudoState {}
sealed case class PlaceHolderState(override val stateName: String) extends PseudoState {}

sealed abstract class BaseTransfer {
  def source: BaseState
  def destination: BaseState
}
sealed case class ConditionalTransfer(val source: BaseState, val destination: BaseState, val cond: Bool) extends BaseTransfer {}
sealed case class UnconditionalTransfer(val source: BaseState, val destination: BaseState) extends BaseTransfer {}

trait Copy[A] {
  def copy: A
}

class FSMDescription() extends Cloneable {
  type NodeType = BaseState
  type EdgeType = BaseTransfer
  type actionType = () => Unit
  type condType = Bool

  // Graph properties
  var nodeList = new mutable.HashSet[NodeType]()
  var entryState: NodeType = EndState
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
  def replaceState(ori: BaseState, state: BaseState) = {
    if (nodeList.exists(_ == ori)) {
      traverseNode(n => n.edgeList = n.edgeList.map({
        case e@ConditionalTransfer(_, des, _) if (des == ori) => e.copy(destination = state)
        case e@UnconditionalTransfer(_, des) if (des == ori) => e.copy(destination = state)
        case otherwise => otherwise
      }))
      nodeList -= ori
      nodeList += state
    }
    state
  }
  def findState(stateName: String): Option[NodeType] = {
    val search = nodeList.filter(_.stateName == stateName)
    search.headOption
  }
  def findOrInsert(name: String) = findOrInsertG(name, new GeneralState(name))
  def findOrHold(name: String) = findOrInsertG(name, new PlaceHolderState(name))
  def findOrInsertG[T <: BaseState](stateName: String, state: T): NodeType = {
    findState(stateName) match {
      case Some(s@PlaceHolderState(name)) => replaceState(s, state)
      case Some(s) => s
      case None    =>
        val s = state
        nodeList += s
        s
    }
  }
  def findInputEdges(s: NodeType): Seq[EdgeType] = {
    val in_e = mutable.ArrayBuffer[EdgeType]()
    traverseEdge(e => if (e.destination == s) {in_e += e})
    in_e
  }
}
