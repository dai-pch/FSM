package libpc.FSM

import chisel3._

class FSMTemplate[IT <: Data, OT <: Data](input_gen: IT, output_gen: OT, des: FSMDescription[IT, OT]) extends Module {
  val io = IO(new Bundle{
    val input = Input(input_gen)
    val output = Output(output_gen)
  })

  val current_state = RegInit(des.encode(des.entryState.get).U(des.state_width))
  val next_state = Wire(UInt(des.state_width.W))

  // state transfer
  current_state := next_state

  // next state logic
  next_state := des.encode(des.entryState.get).U
  for (node <- des.nodeList) {
    when (next_state === des.encode(node).U) {
      node.edgeList.reverse.foreach(_ match {
        case e@ConditionalTransfer(_, dest, _) =>
          when (e.cond(io.input)) {
            next_state := des.encode(dest).U
          }
        case UnconditionalTransfer(_, dest) =>
          next_state := des.encode(dest).U
      })
    }
  }

  // output logic
    //default output
  for (act <- des.defaultAction) {
    act(io.input, io.output)
  }
    // cond
  for (node <- des.nodeList) {
    when (current_state === des.encode(node).U) {
      for (act <- node.actionList.reverse) {
        act(io.input, io.output)
      }
    }
  }
}
