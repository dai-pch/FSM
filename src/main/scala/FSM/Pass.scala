package libpc.FSM

import math._

abstract class FSMPass {
  protected def run(des: FSMDescription): FSMDescription
  def runInternal(des: FSMDescription): FSMDescription
}

trait FSMSimplePass extends FSMPass {
  final override def runInternal(des: FSMDescription): FSMDescription = {
    run(des)
  }
}

trait FSMIteratePass extends FSMPass {
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

object IdlePass extends FSMPass with FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    des.traverseNode(n =>
      n.edgeList = n.edgeList.map({
        case e@ConditionalTransfer(_, EndState, _) => e.copy(destination = des.entryState.get)
        case e@UnconditionalTransfer(_, EndState) => e.copy(destination = des.entryState.get)
        case otherwise => otherwise
      })
    )
    des.nodeList -= EndState()
    des
  }
}

object EncodePass extends FSMPass with FSMSimplePass {
  override protected def run(des: FSMDescription): FSMDescription = {
    des.encode ++= des.nodeList.zipWithIndex
    des.state_width = ceil(log(des.nodeList.size)).toInt
    des
  }
}

object CheckPass extends FSMPass with FSMSimplePass {
  override def run(des: FSMDescription): FSMDescription = {
    assert(des.nodeList.nonEmpty, "FSM is empty.")
    assert(des.encode.size == des.nodeList.size, "FSM is not encoded correctly.")
    assert(des.state_width > 0)
    assert(des.entryState.nonEmpty, "Must indicate entry state.")
    assert(!des.nodeList.exists(EndState()), "Unhandled EndState.")
    des.nodeList.foreach(
      n => assert(n.isInstanceOf[TikState](), "Unhandled PseudoState.")
    )
    des
  }
}

abstract class FSMCompiler {
  val pass: FSMPass
  def compile(des: FSMDescription): FSMDescription = {
    pass.runInternal(des)
  }
}

object IdleFSMCompiler extends FSMCompiler {
  override val pass = FSMPassCompose(
    IdlePass,
    EncodePass,
    CheckPass
  )
}
