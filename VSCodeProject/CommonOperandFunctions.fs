module CommonOperandFunctions

open Machine
open Instruction
open Functions

let registerOperandValue op state =
    State.registerValue op state

let mixedOperandValue op state =
    match op with
    | Register R -> State.registerValue R state
    | Literal x -> x

let execOperandValue op (state:State) =
    match op with
    | MixedOp mop -> mixedOperandValue mop state
    | ExprOp (op2, shift, expr) ->
        let shiftOp = getShiftFunction shift
        shiftOp (State.registerValue op2 state) (mixedOperandValue expr state)

let conditionHolds state cond = 
    match cond with
    | AL -> true
    | EQ -> State.statusBitValue Z state
    | NE -> State.statusBitValue Z state
    | CS | HS -> State.statusBitValue C state
    | _ -> failwithf "conditionHolds error"



