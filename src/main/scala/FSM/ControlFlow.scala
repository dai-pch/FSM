package libpc.FSM

import chisel3._
//import scala.collection.mutable.Stack

class ControlFlow extends FSMBase {
  type actionType = () => Unit
  type condType = desc.condType

  // for state record
  var curState: Option[BaseState] = None
  // for FSM construction


  // help function
  private def gen_name(): String = {
    ""
  }

  //
}
