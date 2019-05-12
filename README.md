# FSM
A Zero Cost Abstruction of FSM(Finite State Machine) circuits based on chisel3.

## A Brief Description
This project is a library based on chisel3, which provides an easy way to construct FSM circuits in chisel by a higher level abstruction with minimal overhead.

It can be used in two mode: the [FSM](#fsm-mode) mode and the [ControlFlow](#control-flow-mode) mode. 

## Add To Dependencies
You should add this library into dependencies of your projects. 

Add following lines into your chisel project:
```

```

## <span id="fsm-mode">FSM Mode</span>
In this mode, user can write an FSM by descriping states and it's actions. 

Here is a simple example that shows you how to construct a FSM.

```scala
import chisel3._
import FSM._

class FSMExample extends Module {
  val io = IO(new Bundle {
    val w_i = Input(Bool())
    val z_o = Output(Bool())
  })

  io.z_o := false.B

  val fsm = InstanciateFSM(new FSM {
    entryState("Idle")
      .act {
        io.z_o := false.B
      }
      .when(io.w_i === true.B).transferTo("s0")

    state("s0")
      .act {
        io.z_o := false.B
      }
      .when(io.w_i === false.B).transferTo("Idle")
      .when(io.w_i === true.B).transferTo("s1")

    state("s1")
      .act {
        io.z_o := true.B
      }
      .when(!io.w_i).transferTo("Idle")
      .otherwise.transferTo("s1")
  })
}
```
First, you should construct a module with io or other signals.
An FSM can be constructed by construct a new instance of `FSM` class.

Second, write FSM description inside FSM class.
- A state can be defined by `state` or `entryState` followed by its name.
- Each FSM must have exactlly one entry state.
- States with the same name will be considered as one state.
- Use `act` method to add actions.
- Use `when` or `otherwise` method to construct a conditional context.
- Use `transferTo` or `transferToEnd` method to descripe a transfer between states.
- `transferToEnd` works the same as `transferTo entry state` in FSM mode. 

Last, use `InstanciateFSM` to launch an FSM.

### Action module
In FSM, an actions is some chisel sentances that will be executed during a state. This is good for combinational logic. 
However, if you want to assign some value to a register, it could be problematic since values will be assigned to the input of a register 
and will only be seen during next clock. That makes it hard to construct a Moore State Machine with some registers as output.

To overcome this problem, FSM provides a method called `actPre`, which has the same type with `act`. 
Actions added by `actPre` will be executed when `next_state == <state_name>` instead of `current_state == <state_name>`, 
makes it easy to operate register in an FSM.

More detiled, the action module of a state is as follows:

Clock | State | Actions to be executed
------|-------|-----------------------
0     | S0    | S0.act                
1     | S0    | S0.act S1.actPre      
2     | S1    | S1.act S1.actPre      
3     | S1    | S1.act S1.actPre      
4     | S1    | S1.act S1.actPre      
5     | S1    | S1.act S2.actPre S2.actLast
6     | S2    | S2.act                
7     | ...   | ...                   


## <span id="control-flow-mode">Control Flow Mode</span>

Coming soon...

