package libpc.FSM

import chisel3._

object Emitter {
  def apply(des: FSMDescription, fsm: FSMBase): Unit = {
    // state register
    val current_state = RegInit(des.encode(des.entryState.get).U(des.state_width.W))
    val next_state = Wire(UInt(des.state_width.W))

    // state transfer
    current_state := next_state

    // next state logic
    next_state := des.encode(des.entryState.get).U
    for (node <- des.nodeList) {
      when (current_state === des.encode(node).U) {
        node.edgeList.reverse.foreach({
          case ConditionalTransfer(src, dest, cond) =>
            assert(src == node)
            when (cond) {
              next_state := des.encode(dest).U
            }
          case UnconditionalTransfer(src, dest) =>
            assert(src == node)
            next_state := des.encode(dest).U
        })
      }
    }

    // output logic
    //default output
//    for (act <- des.defaultAction) {
//      act()
//    }
    // cond
    for (node <- des.nodeList) {
      when (current_state === des.encode(node).U) {
        for (act <- node.asInstanceOf[TikState].actionList.reverse) {
          act()
        }
      }
    }

    // some signal
    fsm.currentState := current_state
    fsm.nextState := next_state
  }
}
