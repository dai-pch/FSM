package fsm.core

import chisel3._
import fsm.core.FSMDescriptionConfig.ActType


object FSMDescriptionConfig {
  type NodeType = BaseState
  type EdgeType = BaseTransfer
  type ActType = () => Unit
  type ActionType = BaseAction
  type ConditionType = Bool
  // configs
  val _endStateName = "_EndState"
}


sealed abstract class BaseAction {
  def act: () => Unit
}

sealed case class NormalAction(act: FSMDescriptionConfig.ActType) extends BaseAction {}
sealed case class PreAction(act: FSMDescriptionConfig.ActType) extends BaseAction {}
sealed case class LastAction(act: FSMDescriptionConfig.ActType) extends BaseAction {}


sealed abstract class BaseState {}

sealed abstract class TikState extends BaseState {}
sealed abstract class PseudoState extends BaseState {}

sealed case class GeneralState(
                                actionList: Array[FSMDescriptionConfig.ActionType] = Array(),
                                last_flag: Bool = Wire(Bool())
                              ) extends TikState {
  last_flag := false.B
//  def this(act: FSMDescriptionConfig.ActType) = this(Array(NormalAction(act)))
  def addAct(act: FSMDescriptionConfig.ActionType): GeneralState = {
    copy(actionList = actionList :+ act)
  }
  def addAct(act: FSMDescriptionConfig.ActType): GeneralState = {
    addAct(NormalAction(act))
  }
}
sealed case class SubFSMState(fsm: FSMBase) extends TikState {}
//  class StartState() extends BaseState {}
case object EndState extends PseudoState {}
sealed case class SkipState() extends PseudoState {}
sealed case class PlaceHolderState() extends PseudoState {}


sealed abstract class BaseTransfer {
  val source: String
  val destination: String
  val actions: BaseTransfer.ActionsType
}
object BaseTransfer {
  type ActionsType = Array[FSMDescriptionConfig.ActType]
  def unapply(arg: BaseTransfer): Option[(String, String, ActionsType)] =
    Some(arg.source, arg.destination, arg.actions)
}
sealed case class ConditionalTransfer(override val source: String,
                                      override val destination: String,
                                      cond: FSMDescriptionConfig.ConditionType,
                                      override val actions: BaseTransfer.ActionsType = Array()
                                     ) extends BaseTransfer {}
sealed case class UnconditionalTransfer(override val source: String,
                                        override val destination: String,
                                        override val actions: BaseTransfer.ActionsType = Array()
                                       ) extends BaseTransfer {}


object FSMDescription {
  def apply(): FSMDescription = {
    new FSMDescription(
      Map(FSMDescriptionConfig._endStateName -> EndState),
      Array(),
      FSMDescriptionConfig._endStateName,
      Map(),
      0
    )
  }
}

case class FSMDescription(// Graph properties
                          private val nodeMap: Map[String, FSMDescriptionConfig.NodeType],
                          private val edgeArray: Array[FSMDescriptionConfig.EdgeType],
                          entryState: String,
                          // compile info
                          encode: Map[String, Int],
                          state_width: Int
                         ) {
  //type info
  type NodeType = FSMDescriptionConfig.NodeType
  type NodeMap = (String, NodeType)
  type EdgeType = FSMDescriptionConfig.EdgeType
  type ActType = FSMDescriptionConfig.ActType
  type ActionType = FSMDescriptionConfig.ActionType
  type ConditionType = FSMDescriptionConfig.ConditionType
  // class methods
  def conditionalTransferTo(src: String, dest: String, cond: ConditionType, acts: BaseTransfer.ActionsType = Array()): FSMDescription = {
    copy(edgeArray = edgeArray :+ ConditionalTransfer(src, dest, cond, acts))
  }
  def unconditionalTransferTo(src: String, dest: String, acts: BaseTransfer.ActionsType = Array()): FSMDescription = {
    copy(edgeArray = edgeArray :+ UnconditionalTransfer(src, dest, acts))
  }
  def replaceState(ori: String, state: BaseState): FSMDescription = {
    assert(nodeMap contains ori)
    copy(nodeMap = nodeMap + (ori -> state))
  }
  def setEntry(entryStateName: String): FSMDescription = {
    copy(entryState = entryStateName)
  }
  // node
  def addAct(name: String, act: ActionType): FSMDescription = {
    assert((nodeMap contains name) && nodeMap(name).isInstanceOf[GeneralState], "Can not add actions into pseudo state.")
    val node = nodeMap(name).asInstanceOf[GeneralState]
    replaceState(name, node.addAct(act))
  }
  def addAct(name: String, act: ActType): FSMDescription = {
    addAct(name, NormalAction(act))
  }
  def addPre(name: String, act: ActType): FSMDescription = {
    addAct(name, PreAction(act))
  }
  def addLast(name: String, act: ActType): FSMDescription = {
    addAct(name, LastAction(act))
  }
  def insertIfNotFound(name: String): FSMDescription = insertIfNotFoundG(name, GeneralState())
  def holdIfNotFound(name: String): FSMDescription = insertIfNotFoundG(name, PlaceHolderState())
  def insertIfNotFoundG[T <: BaseState](stateName: String, state: T): FSMDescription = {
    nodeMap.get(stateName) match {
      case Some(PlaceHolderState()) => replaceState(stateName, state)
      case Some(s) => this
      case None    => this + (stateName, state)
    }
  }
  def procNode(name: String, f: NodeType => NodeType): FSMDescription = {
    mapNodes(_._1 == name, x => (x._1, f(x._2)))
  }
  def filterNodes(cond: NodeMap => Boolean): FSMDescription = {
    this.copy(nodeMap = nodeMap.toArray.filter(cond).toMap)
  }
  def mapNodes(cond: NodeMap => Boolean, func: NodeMap => NodeMap): FSMDescription = {
    mapNodesSeq(cond, Array apply func(_))
  }
  def mapNodesSeq(cond: NodeMap => Boolean, func: NodeMap => Seq[NodeMap]): FSMDescription = {
    this.copy(nodeMap = nodeMap.toArray.map({
      case (n, s) if cond(n, s) => func(n, s).toArray[NodeMap]
      case other => Array(other)
    }).reduce(_++_).toMap)
  }
//  def traverseNode(func: NodeMap => NodeType): FSMDescription = {
//    this.copy(nodeMap = nodeMap.toArray.map({case (k, v) => (k, func(k, v))}).toMap)
//  }
//  def procNode(state: String, func: NodeType => NodeType): FSMDescription = {
//    this.copy(nodeMap = nodeMap + (state -> func(nodeMap(state))))
//  }
  def +(stateName: String, state: NodeType): FSMDescription = {
    assert(!nodeMap.contains(stateName), s"States $stateName alrady exists.")
    this.copy(nodeMap = nodeMap + (stateName -> state))
  }
  def ++(states: Seq[NodeMap]): FSMDescription = {
    states.foreach(x => assert(!nodeMap.contains(x._1)))
    this.copy(nodeMap = nodeMap ++ states)
  }
  def -(stateName: String): FSMDescription = {
    this.copy(nodeMap = nodeMap - stateName)
  }
  def --(stateNames: Seq[String]): FSMDescription = {
    this.copy(nodeMap = nodeMap -- stateNames)
  }
  def containsState(stateName: String): Boolean = {
    nodeMap contains stateName
  }
  def findState(stateName: String): Option[NodeType] = {
    if (containsState(stateName))
      Some(nodeMap(stateName))
    else
      None
  }
  def statesOfType[T <: NodeType](dummy: T):Array[(String, T)] = {
    nodes.filter(_._2.getClass() == dummy.getClass() )
      .map(x => (x._1, x._2.asInstanceOf[T]))
  }
  def nodes: Array[NodeMap] = {
    nodeMap.toArray
  }
  def nodeNames: Array[String] = {
    nodes.map(_._1)
  }
  // edge
  def traverseEdge(func: EdgeType => EdgeType): FSMDescription = {
    this.copy(edgeArray = edgeArray.map(func))
  }
  def mapEdge(cond: EdgeType => Boolean, func: EdgeType => EdgeType): FSMDescription = {
    val _f: EdgeType => Seq[EdgeType] = Array apply func(_)
    mapEdgeSeq(cond, _f)
  }
  def mapEdgeSeq(cond: EdgeType => Boolean, func: EdgeType => Seq[EdgeType]): FSMDescription = {
    copy(edgeArray = edgeArray.foldLeft(Array[EdgeType]())({
      case (res, e) if cond(e) => res ++ func(e)
      case (res, e) => res :+ e
    }))
  }
  def +~(edge: EdgeType): FSMDescription = {
    copy(edgeArray = edgeArray :+ edge)
  }
  def ++~(edges: Seq[EdgeType]): FSMDescription = {
    copy(edgeArray = edgeArray ++ edges)
  }
  def -~(edge: EdgeType): FSMDescription = {
    this.copy(edgeArray = edgeArray.filterNot(_ == edge))
  }
  def --~(edges: Seq[EdgeType]): FSMDescription = {
    this.copy(edgeArray = edgeArray.filterNot(edges.contains(_)))
  }
  def edgesFrom(name: String): Array[EdgeType] = {
    edgeArray.filter(_.source == name)
  }
  def edgesTo(name: String): Array[EdgeType] = {
    edgeArray.filter(_.destination == name)
  }
  def edges: Array[EdgeType] = {
    edgeArray
  }
  // other help function
  def renameNode(origin: String, name: String): FSMDescription = {
    this.copy(
      nodeMap = nodeMap - origin + (name -> nodeMap(origin) ),
      edgeArray = edgeArray.map({
        case e@ConditionalTransfer(src, _, _, _) if src == origin => e.copy(source = name)
        case e@UnconditionalTransfer(src, _, _) if src == origin => e.copy(source = name)
        case e => e
      })
      .map({
        case e@ConditionalTransfer(_, dest, _, _) if dest == origin => e.copy(destination = name)
        case e@UnconditionalTransfer(_, dest, _) if dest == origin => e.copy(destination = name)
        case e => e
      })
    )
  }
  def renameNodes(cond: NodeMap => Boolean, func: String => String): FSMDescription = {
    val origins = nodeMap.toArray.filter(cond).map(_._1)
    var desc = this
    for (name <- origins) {
      desc = desc.renameNode(name, func(name))
    }
    desc
  }

  override def toString: String = {
    "Nodes: " + nodeMap.toString() + ", \nEdges: " + edgeArray.map(_.toString()).reduce(_+ ", " +_)
  }
}
