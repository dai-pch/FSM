package libpc.FSM

import chisel3._
import chisel3.util.log2Ceil
import scala.collection.mutable

abstract class FSMPass {
  protected def run(des: FSMDescription): FSMDescription
  def runInternal(des: FSMDescription): FSMDescription
}

abstract class FSMSimplePass extends FSMPass {
  final override def runInternal(des: FSMDescription): FSMDescription = {
    run(des)
  }
}

abstract class FSMIteratePass extends FSMPass {
  val maxRun = 5000
  final override def runInternal(des: FSMDescription): FSMDescription = {
    var count = 0
    var old = des
    var n: FSMDescription = old.copy()
    do {
      run(n)
      count += 1
      if (count > maxRun)
        throw new FSMCompileNoStopping
      old = n
      n = old.copy()
    } while (n != old)
    n
  }
}

class FSMComposePass(val passList: Seq[FSMPass]) extends FSMSimplePass {
  def run(des: FSMDescription): FSMDescription = {
    var fsm = des
    for (pass <- passList)
      fsm = pass.runInternal(fsm)
    fsm
  }
}

object FSMPassCompose {
  def apply(passes: FSMPass*): FSMComposePass = new FSMComposePass(passes)
}

object MergeSkipPass extends FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    val skipStates = des.nodeList.filter(_.isInstanceOf[SkipState])
    for (skipState <- skipStates) {
      val inEdges = des.findInputEdges(skipState)
      for (e <- inEdges) {
        val source = e.source
        val e_cond = if (e.isInstanceOf[ConditionalTransfer]) Some(e.asInstanceOf[ConditionalTransfer].cond) else None
        val e_id = source.edgeList.indexOf(e)
        if (e_id == -1) throw new FSMCompileEdgeNotFound
        source.edgeList.insertAll(e_id, skipState.edgeList.map(
            se => {(se, e_cond) match {
              case (ConditionalTransfer(s, dest, cond), Some(e_c)) =>
                assert(s == skipState)
                ConditionalTransfer(source, dest, cond && e_c)
              case (UnconditionalTransfer(s, dest), Some(e_c)) =>
                assert(s == skipState)
                ConditionalTransfer(source, dest, e_c)
              case (ConditionalTransfer(s, dest, cond), None) =>
                assert(s == skipState)
                ConditionalTransfer(source, dest, cond)
              case (UnconditionalTransfer(s, dest), None) =>
                assert(s == skipState)
                UnconditionalTransfer(source, dest)
            }}
        ))
        source.edgeList -= e
      }
    }
    des
  }
}

object DeleteUnreachableStatePass extends FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    val reachable = mutable.HashSet[BaseState](des.entryState)
    val queue = mutable.Queue[BaseState](des.entryState)
    while (queue.nonEmpty) {
      val cur = queue.dequeue()
      cur.edgeList.map(_.destination).foreach(
        n => if (!reachable.exists(_ == n)) {
          reachable += n
          queue.enqueue(n)
        }
      )
    }
    des.nodeList = reachable
    des
  }
}

object IdlePass extends FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    des.traverseNode(n =>
      n.edgeList = n.edgeList.map({
        case e@ConditionalTransfer(_, EndState, _) => e.copy(destination = des.entryState)
        case e@UnconditionalTransfer(_, EndState) => e.copy(destination = des.entryState)
        case otherwise => otherwise
      })
    )
    des.nodeList -= EndState
    des
  }
}

object EncodePass extends FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    des.encode ++= des.nodeList.zipWithIndex
    println(des.encode)
    val state_w = log2Ceil(des.nodeList.size)
    des.state_width = state_w
    des
  }
}

object CheckPass extends FSMSimplePass {
  override def run(des: FSMDescription): FSMDescription = {
    assert(des.nodeList.nonEmpty, "FSM is empty.")
    assert(des.encode.size == des.nodeList.size, "FSM is not encoded correctly.")
    assert(des.state_width > 0)
    assert(des.entryState != EndState, "Must indicate entry state.")
    assert(!des.nodeList.exists(_ == EndState), "Unhandled EndState.")
    des.nodeList.foreach(
      n => assert(n.isInstanceOf[TikState], "Unhandled PseudoState.")
    )
    des
  }
}

object PreprocessPass extends FSMComposePass(Seq(
  MergeSkipPass
))

object OptimizePass extends FSMComposePass(Seq(
  DeleteUnreachableStatePass
))

abstract class FSMCompiler {
  val pass: FSMPass
  def compile(des: FSMDescription): FSMDescription = {
    pass.runInternal(des)
  }
}

object IdleFSMCompiler extends FSMCompiler {
  override val pass = FSMPassCompose(
    PreprocessPass,
    OptimizePass,
    IdlePass,
    EncodePass,
    CheckPass
  )
}
