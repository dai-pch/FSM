package libpc.FSM

import chisel3._

object Emitter {
  def apply(des: FSMDescription, fsm: FSMBase): Unit = {
    // state register
    val current_state = RegInit(des.encode(des.entryState).U(des.state_width.W))
    val next_state = Wire(UInt(des.state_width.W))

    // state transfer
    current_state := next_state

    // next state logic
    next_state := current_state
    for ((node, _) <- des.nodes) {
      when (current_state === des.encode(node).U) {
        for (e <- des.edgesFrom(node).reverse) {
          e match {
            case ConditionalTransfer(src, dest, cond) =>
              assert(src == node)
              when (cond) {
                next_state := des.encode(dest).U
              }
            case UnconditionalTransfer(src, dest) =>
              assert(src == node)
              next_state := des.encode(dest).U
          }
        }
      }
    }

    // output logic
    for ((name, state) <- des.nodes) {
      for (action <- state.asInstanceOf[GeneralState].actionList.reverse) {
        action match {
          case NormalAction(act) =>
            when (current_state === des.encode(name).U) {
              act()
            }
          case PreAction(act) =>
            when (next_state === des.encode(name).U) {
              act()
            }
          case LastAction(act) =>
            when ((current_state === des.encode(name).U) && state.asInstanceOf[GeneralState].last_flag) {
              act()
            }
        }
      }
    }
    //

    // some signal
    fsm.current_state := current_state
    fsm.current_state.state_conds.foreach({
      case (name, sig) => sig := (current_state === des.encode(name).U)
    })
    fsm.next_state := next_state
    fsm.next_state.state_conds.foreach({
      case (name, sig) => sig := (next_state === des.encode(name).U)
    })

    //debug
//    printf(p"current state $current_state \n")
//    printf(p"next state $next_state \n")
  }
}
