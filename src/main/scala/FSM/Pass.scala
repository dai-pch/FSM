package libpc.FSM

import math._

abstract class FSMPass {
  protected def run[IT, OT](des: FSMDescription[IT, OT]): FSMDescription[IT, OT]
  def runInternal[IT, OT](des: FSMDescription[IT, OT]): FSMDescription[IT, OT]
}

trait FSMSimplePass extends FSMPass {
  final override def runInternal[IT, OT](des: FSMDescription[IT, OT]): FSMDescription[IT, OT] = {
    run(des)
  }
}

trait FSMIteratePass extends FSMPass {
  val maxRun = 5000
  final override def runInternal[IT, OT](des: FSMDescription[IT, OT]): FSMDescription[IT, OT] = {
    var count = 0
    var old = des
    var n: FSMDescription[IT, OT] = old.copy()
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
  def run[IT, OT](des: FSMDescription[IT, OT]): FSMDescription[IT, OT] = {
    var fsm = des
    for (pass <- passList)
      fsm = pass.runInternal(fsm)
    fsm
  }
}

object FSMComposePass {
  def apply(passes: FSMPass*): FSMComposePass = new FSMComposePass(passes)
}

object IdlePass extends FSMPass with FSMSimplePass {
  override protected def run[IT, OT](des: FSMDescription[IT, OT]): FSMDescription[IT, OT] = {
    des.traverseNode(n =>
      n.edgeList = n.edgeList.map(_ match {
        case e@des.Edge(_, des.EndState(), _) => e.copy(destination = des.entryState.get)
        case otherwise => otherwise
      }))
    des.nodeList -= des.EndState()
    des
  }
}

object EncodePass extends FSMPass with FSMSimplePass {
  override protected def run[IT, OT](des: FSMDescription[IT, OT]): FSMDescription[IT, OT] = {
    des.encode ++= des.nodeList.zipWithIndex
    des.state_width = ceil(log(des.nodeList.size)).toInt
    des
  }
}

object CheckPass extends FSMPass with FSMSimplePass {
  override def run[IT, OT](des: FSMDescription[IT, OT]): FSMDescription[IT, OT] = {
    assert(des.nodeList.nonEmpty, "FSM is empty.")
    assert(des.encode.size == des.nodeList.size, "FSM is not encoded correctly.")
    assert(des.state_width > 0)
    assert(des.entryState.nonEmpty, "Must indicate entry state.")
    assert(des.nodeList.find(_ == des.EndState()).isEmpty, "Unhandled EndState.")
    des
  }
}

abstract class FSMCompiler {
  val pass: FSMPass
  def compile[IT, OT](des: FSMDescription[IT, OT]): FSMDescription[IT, OT] = {
    pass.runInternal(des)
  }
}

object IdleFSMCompile extends FSMCompiler {
  override val pass = FSMComposePass(
    IdlePass,
    EncodePass,
    CheckPass
  )
}
