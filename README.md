# FSM
A Zero Cost Abstruction of FSM(Finite State Machine) circuits based on chisel3.

## A Brief Description
This project is a library based on chisel3, which provides an easy way to construct FSM circuits in chisel by a higher level abstruction with minimal overhead.

It can be used in two mode: the [FSM](#fsm-mode) mode and the [ControlFlow](#control-flow-mode) mode. 

## Add To Dependencies
You should add this library into dependencies of your projects. 

Add following lines into your chisel project:
```
lazy val FSMGenerator = RootProject(uri("git://github.com/dai-pch/FSM.git#master"))
<your project>.dependsOn(FSMGenerator)
```

## <span id="fsm-mode">FSM Mode</span>
In this mode, user can write an FSM by descriping states and it's actions. 

Here is a simple example that shows you how to construct a FSM.

```scala
import chisel3._
import fsm._

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
In FSM, an action is a group of chisel sentance that will be executed during a state. This is fine for combinational logic. 
However, if you want to assign some value to a register, it could be problematic. Because in chisel's hardware module, values will be assigned to the input of a register 
and can only be seen the next clock. That makes it hard to construct a Moore State Machine with some registers as outputs.

To overcome this problem, FSM provides a method called `actPre`, which can be used the same with `act`. 
Actions added by `actPre` will be executed when `next_state == <state_name>` instead of `current_state == <state_name>`, 
makes it easy to operate register in an FSM.

More detiled, the action module is as follows:

Clock | State | Actions to be executed
------|-------|-----------------------
0     | S0    | S0.act                
1     | S0    | S0.act        S1.actPre      
2     | S1    |        S1.act S1.actPre      
3     | S1    |        S1.act S1.actPre      
4     | S1    |        S1.act S1.actPre      
5     | S1    |        S1.act           S1.actLast S2.actPre
6     | S2    |                                              S2.act                
7     | ...   | ...                   


## <span id="control-flow-mode">Control Flow Mode</span>

### Example
Here is an example for construct FSM using `ControlFlow` mode.

```scala
import fsm._
import chisel3._

class CFExample extends Module {
  val io = IO(new Bundle{
    val in = Input(Bool())
    val output = Output(Bool())
  })

  io.output := false.B

  val fsm = InstanciateFSM(new ControlFlow {
    start {
      io.output := false.B
    }

    tick {
      io.output := true.B
    }.tag("tag1")

    run {
      tick {
        io.output := false.B
      }

      tick {
        io.output := true.B
      }
    }.until(io.in)

    loop(!io.in) {
      tick {
        io.output := false.B
      }
    }

    repeat(3) {
      tick {
        io.output := true.B
      }
    }

    branch(io.in) {
      tick {}
    }.or_branch(!io.in) {
      tick {}
    }.or {
      goto("tag1")
    }

    subFSM(new FSM {
      entryState("subStart")
        .otherwise.transferToEnd
    })

    end
  })
}
```
In `ControlFlow` mode, each actions are blocked by `tick` method, which generate one state.
All the `tick` will be executed in order.
FSM also provides methods for loop and branch structures.

### Loop
FSM provides `loop`, `run-until` and `repeat` methods for constructing loop structure.

`loop` works like `while` in C languages. It will run its contents whenever the condition signal is true.

`run-until` works like `do-while` in C languages. It will run one or more times determined by the condition signal.

`repeat` only copy the state multiple times.

Each loop method must contains at least one tick structures.

### Branch
FSM provides `branch`, `or_branch` and `or` for branch structures. They are like `if`, `else-if` and `else` in C language.
