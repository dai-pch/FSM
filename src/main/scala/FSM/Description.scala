package libpc.FSM

import chisel3._


object FSMDescriptionConfig {
  type NodeType = BaseState
  type EdgeType = BaseTransfer
  type ActionType = () => Unit
  type ConditionType = Bool
  // configs
  val _endStateName = "_EndState"
}

sealed abstract class BaseState {}

sealed abstract class TikState extends BaseState {}
sealed abstract class PseudoState extends BaseState {}

sealed case class GeneralState(actionList: Array[FSMDescriptionConfig.ActionType] = Array()) extends TikState {
  def addAct(act: FSMDescriptionConfig.ActionType) = {
    copy(actionList = actionList :+ act)
  }
}
sealed case class SubFSMState(fsm: FSMBase) extends TikState {}
//  class StartState() extends BaseState {}
case object EndState extends PseudoState {}
sealed case class SkipState() extends PseudoState {}
sealed case class PlaceHolderState() extends PseudoState {}

sealed abstract class BaseTransfer(val source: String, val destination: String) {}
object BaseTransfer {
  def unapply(arg: BaseTransfer): Option[(String, String)] = Some(arg.source, arg.destination)
}
sealed case class ConditionalTransfer(override val source: String,
                                      override val destination: String,
                                      cond: FSMDescriptionConfig.ConditionType
                                     ) extends BaseTransfer(source, destination) {}
sealed case class UnconditionalTransfer(override val source: String,
                                        override val destination: String
                                       ) extends BaseTransfer(source, destination) {}


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
  type ActionType = FSMDescriptionConfig.ActionType
  type ConditionType = FSMDescriptionConfig.ConditionType
  // class methods
  def conditionalTransferTo(src: String, dest: String, cond: ConditionType): FSMDescription = {
    copy(edgeArray = edgeArray :+ ConditionalTransfer(src, dest, cond))
  }
  def unconditionalTransferTo(src: String, dest: String): FSMDescription = {
    copy(edgeArray = edgeArray :+ UnconditionalTransfer(src, dest))
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
  def insertIfNotFound(name: String): FSMDescription = insertIfNotFoundG(name, GeneralState())
  def holdIfNotFound(name: String): FSMDescription = insertIfNotFoundG(name, PlaceHolderState())
  def insertIfNotFoundG[T <: BaseState](stateName: String, state: T): FSMDescription = {
    nodeMap.get(stateName) match {
      case Some(PlaceHolderState()) => replaceState(stateName, state)
      case Some(s) => this
      case None    => this.copy(nodeMap = nodeMap + (stateName -> state))
    }
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
    assert(!nodeMap.contains(stateName))
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
    nodes.filter(_._2.isInstanceOf[T])
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
        case e@ConditionalTransfer(src, _, _) if src == origin => e.copy(source = name)
        case e@UnconditionalTransfer(src, _) if src == origin => e.copy(source = name)
        case e => e
      })
      .map({
        case e@ConditionalTransfer(_, dest, _) if dest == origin => e.copy(destination = name)
        case e@UnconditionalTransfer(_, dest) if dest == origin => e.copy(destination = name)
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
}
