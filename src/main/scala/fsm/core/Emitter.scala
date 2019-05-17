package fsm.core

import chisel3._
import chisel3.util.{switch, is}

object Emitter {
  def apply(fsm: FSMBase): Unit = {
    val desc = fsm.desc
    // state register
    val current_state = RegInit(desc.encode(desc.entryState).U(desc.state_width.W))
    val next_state = Wire(UInt(desc.state_width.W))

    // default actions
    for (act <- fsm.default_actions) {
      act()
    }

    // fork fsms
    fsm.fork_fsms.foreach(Emitter(_))

    // state transfer
    current_state := next_state

    next_state := current_state
    for ((state_name, state) <- desc.nodes.map(x => (x._1, x._2.asInstanceOf[GeneralState]))) {
      when(current_state === desc.encode(state_name).U) {
        // output logic
        // normal act
        for (act <- state.actionList.filter(_.isInstanceOf[NormalAction]).reverse.map(_.act)) {
          act()
        }
        // last act
        when(state.last_flag) {
          for (act <- state.actionList.filter(_.isInstanceOf[LastAction]).reverse.map(_.act)) {
            act()
          }
        }
        // next state logic and edge actions logic
        var when_ctx: WhenContext = null
        for (e <- desc.edgesFrom(state_name)) {
          e match {
            case ConditionalTransfer(src, dest, cond, acts) =>
              assert(src == state_name)
              if (when_ctx == null) {
                when_ctx = when(cond) {
                  for (e_act <- acts) {
                    e_act()
                  }
                  next_state := desc.encode(dest).U
                }
              } else {
                when_ctx = when_ctx.elsewhen(cond) {
                  for (e_act <- acts) {
                    e_act()
                  }
                  next_state := desc.encode(dest).U
                }
              }
            case UnconditionalTransfer(src, dest, acts) =>
              assert(src == state_name)
              if (when_ctx == null) {
                for (e_act <- acts) {
                  e_act()
                }
                next_state := desc.encode(dest).U
              } else {
                when_ctx.otherwise {
                  for (e_act <- acts) {
                    e_act()
                  }
                  next_state := desc.encode(dest).U
                }
              }
          }
        }
      }
    }

    // pre act
    for ((name, state) <- desc.nodes) {
      when(next_state === desc.encode(name).U) {
        for (act <- state.asInstanceOf[GeneralState].actionList.reverse.filter(_.isInstanceOf[PreAction]).map(_.act)) {
          act()
        }
      }
    }
    //

    // some signal
    fsm.current_state := current_state
    fsm.current_state.state_conds.foreach({
      case (name, sig) => sig := (current_state === desc.encode(name).U)
    })
    fsm.next_state := next_state
    fsm.next_state.state_conds.foreach({
      case (name, sig) => sig := (next_state === desc.encode(name).U)
    })

    //debug
//    printf(p"current state $current_state \n")
//    printf(p"next state $next_state \n")
  }
}
