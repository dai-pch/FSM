package libpc.FSM

import chisel3._
import chisel3.util.log2Ceil
import scala.collection.mutable

abstract class FSMPass {
  //type info
  type NodeType = FSMDescriptionConfig.NodeType
  type EdgeType = FSMDescriptionConfig.EdgeType
  type ActionType = FSMDescriptionConfig.ActType
  type ConditionType = FSMDescriptionConfig.ConditionType
  //
  var debug: Boolean = false
  protected def run(des: FSMDescription): FSMDescription
  def runInternal(des: FSMDescription): FSMDescription
}

abstract class FSMSimplePass extends FSMPass {
  def apply(debug_ : Boolean = false): this.type = {
    debug = debug_
    this
  }

  final override def runInternal(des: FSMDescription): FSMDescription = {
    if (debug) {
      println(s"Before pass ${this.getClass.getName}.")
      println(des.toString())
    }
    val desc = run(des)
    if (debug) {
      println(s"After pass ${this.getClass.getName}.")
      println(desc.toString())
    }
    desc
  }
}

abstract class FSMIteratePass extends FSMPass {
  val maxRun = 5000
  final override def runInternal(des: FSMDescription): FSMDescription = {
    var count = 0
    var old_des: FSMDescription = null
    var new_des = des
    do {
      old_des = new_des
      new_des = run(old_des)
      count += 1
      if (count > maxRun)
        throw new FSMCompileNoStopping
    } while (old_des != new_des)
    new_des
  }
}

class FSMComposePass(val passList: Seq[FSMPass], debug_ : Boolean = false) extends FSMSimplePass {
  debug = debug_
  def run(des: FSMDescription): FSMDescription = {
    var fsm = des
    for (pass <- passList) {
      pass.debug = debug
      fsm = pass.runInternal(fsm)
    }
    fsm
  }
}

object FSMPassCompose {
  def apply(passes: FSMPass*): FSMComposePass = new FSMComposePass(passes)
  def apply(debug: Boolean, passes: FSMPass*): FSMComposePass = new FSMComposePass(passes, debug)
}

object AssignLastFlagPass extends FSMSimplePass {
  override protected def run(description_ : FSMDescription): FSMDescription = {
    val desc = description_
    desc.nodes.foreach({
      case (name, GeneralState(_, last)) => last := desc.edgesFrom(name).foldRight(false.B)({
        case (ConditionalTransfer(_, des, cond), r) if des != name => cond || r
        case (UnconditionalTransfer(_, des), _) if des != name => true.B
        case (ConditionalTransfer(_, des, cond), r) if des == name => !cond && r
        case (UnconditionalTransfer(_, des), _) if des == name => false.B
      })
      case (_, SubFSMState(fsm)) => fsm.desc = run(fsm.desc)
      case others => Unit
    })
    desc
  }
}

object MergeSubFSMPass extends FSMSimplePass {
  override protected def run(description_ : FSMDescription): FSMDescription = {
    var desc = description_
    val subs = desc.statesOfType(SubFSMState(null))
    for ((name, sub_state) <- subs) {
      var sub_desc = sub_state.fsm.desc
      val addPrefix = name + "_" + _
      sub_desc = sub_desc.replaceState(FSMDescriptionConfig._endStateName, SkipState())
      sub_desc = sub_desc.renameNodes(_=>true, addPrefix)
      desc = desc ++ sub_desc.nodes
      desc = desc ++~ sub_desc.edges
      desc = desc
        .mapEdge(_.destination == name, {
          case e@ConditionalTransfer(_, _, _) => e.copy(destination = addPrefix(sub_desc.entryState) )
          case e@UnconditionalTransfer(_, _) => e.copy(destination = addPrefix(sub_desc.entryState) )
        })
        .mapEdge(_.source == name, {
          case e@ConditionalTransfer(_, _, _) => e.copy(source = addPrefix(FSMDescriptionConfig._endStateName) )
          case e@UnconditionalTransfer(_, _) => e.copy(source = addPrefix(FSMDescriptionConfig._endStateName) )
        })
    }
    desc
  }
}

object MergeSkipPass extends FSMSimplePass {
  override protected def run(desc: FSMDescription): FSMDescription = {
    var new_desc = desc
    val skipStates = new_desc.nodes.filter(_._2.isInstanceOf[SkipState]).map(_._1)
    for (name <- skipStates) {
      val out_edges = new_desc.edgesFrom(name)
      new_desc = new_desc.mapEdgeSeq(name == _.destination, {
        case ConditionalTransfer(src, _, cond) => out_edges.map({
          case ConditionalTransfer(_, dest, cond2) => ConditionalTransfer(src, dest, cond && cond2)
          case UnconditionalTransfer(_, dest) => ConditionalTransfer(src, dest, cond)
        })
        case UnconditionalTransfer(src, _) => out_edges.map({
          case ConditionalTransfer(_, dest, cond2) => ConditionalTransfer(src, dest, cond2)
          case UnconditionalTransfer(_, dest) => UnconditionalTransfer(src, dest)
        })
      })
      new_desc = new_desc --~ out_edges
    }
    new_desc
  }
}

object DeleteUnreachableEdgePass extends FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    val unreachable_edges = mutable.ArrayBuffer[EdgeType]()
    for ((name, s) <- des.nodes) {
      val edges = des.edgesFrom(name)
      val (should_drop, _) = edges.foldLeft((Array[EdgeType](), false))({
        case ((array, true), e) => (array :+ e, true)
        case ((array, _), e@UnconditionalTransfer(_, _)) => (array, true)
        case ((array, _), e) => (array, false)
      })
      unreachable_edges ++= should_drop
    }
    des --~ unreachable_edges
  }
}

object DeleteUnreachableStatePass extends FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    val reachable = mutable.Set[String](des.entryState)
    val queue = mutable.Queue[String](des.entryState)
    while (queue.nonEmpty) {
      val cur = queue.dequeue()
      for (name <- des.edgesFrom(cur).map(_.destination)) {
        if (!reachable.contains(name)) {
          reachable += name
          queue.enqueue(name)
        }
      }
    }
    des.filterNodes(x => reachable.contains(x._1))
  }
}

object IdlePass extends FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    var new_desc = des
    new_desc = new_desc.replaceState(FSMDescriptionConfig._endStateName, SkipState())
    new_desc = new_desc +~ UnconditionalTransfer(FSMDescriptionConfig._endStateName, des.entryState)
    new_desc
  }
}

object EncodePass extends FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    val enc = des.nodes.map(_._1).zipWithIndex
    var new_desc = des.copy(encode = enc.toMap)
    val state_w = log2Ceil(enc.length)
    new_desc.copy(state_width = state_w)
  }
}

object CheckPass extends FSMSimplePass {
  override def run(des: FSMDescription): FSMDescription = {
    assert(des.nodes.nonEmpty, "FSM is empty.")
    assert(des.encode.size == des.nodes.length, "FSM is not encoded correctly.")
    assert(des.state_width > 0)
    assert(des.entryState != FSMDescriptionConfig._endStateName, "Must indicate entry state.")
    assert(!des.nodes.map(_._1).contains(FSMDescriptionConfig._endStateName), "Unhandled EndState.")
    des.nodes.foreach({
      case (n, s) => assert(s.isInstanceOf[TikState], s"Unhandled PseudoState $s: $n.")
    })
    des
  }
}

object PreprocessPass extends FSMComposePass(Seq(
  AssignLastFlagPass,
  MergeSubFSMPass,
  MergeSkipPass
))

object OptimizePass extends FSMComposePass(Seq(
  DeleteUnreachableEdgePass,
  DeleteUnreachableStatePass
))

abstract class FSMCompiler {
  val pass: FSMPass
  def compile(des: FSMDescription): FSMDescription = {
    pass.runInternal(des)
  }
}

object IdleFSMCompiler extends FSMCompiler {
  def apply(debug : Boolean = false): this.type = {
    pass.debug = debug
    this
  }
  override val pass = FSMPassCompose(
    IdlePass,
    PreprocessPass,
    OptimizePass,
    EncodePass,
    CheckPass
  )
}
