module Execution

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

let conditionHolds = function
    | AL -> true
    | EQ -> statusRegister.Z
    | NE -> statusRegister.Z
    | CS | HS -> statusRegister.C
    | _ -> false


// let checkCarry (add:bool) (result:int) =
//     if add && result >= (1<<<32) then
//         1
//     elif not <| add && result >= 0 then
//         0
//     else
//         0

// let checkOverflow () =
//     0

let executeArithmetic (operation:ArithmeticOperation)
                      (operands:ArithmeticOperandsPattern)
                      (state:State) =
    let core, S, cond = operation
    match conditionHolds cond with
    | true ->
        let dest, op1, op2 = operands
        let op1Val = State.registerValue op1 state
        let op2Val = execOperandValue op2 state
        let result = getArithmeticFunction core <| op1Val <| op2Val
        // Need to update CSPR here
        State.updateRegister dest result state
    | false ->
        state

let executeShift (operation:ShiftOperation)
                 (operands:ShiftOperandsPattern)
                 (state:State) =
    let core, S, cond = operation
    if conditionHolds cond then
        let dest, op1, op2 = operands
        let op1Val = State.registerValue op1 state
        let op2Val = mixedOperandValue op2 state
        let result = getShiftFunction core <| op1Val <| op2Val
        // Need to update CSPR here
        State.updateRegister dest result state
    else
        state

let executeCompare (operation:CompareOperation)
                   (operands:CompareOperandsPattern)
                   (state:State) =
    let core, cond = operation
    if conditionHolds cond then
        let op1, op2 = operands
        let op1Val = State.registerValue op1 state
        let op2Val = execOperandValue op2 state
        let result = getCompareFunction core <| op1Val <| op2Val
        // Need to update CSPR here
        state
    else
        state

let executeBitwise (operation:BitwiseOperation)
                   (operands:BitwiseOperandsPattern)
                   (state:State) =
    let core, S, cond = operation
    match conditionHolds cond with
    | true ->
        let dest, op1, op2 = operands
        let op1Val = State.registerValue op1 state
        let op2Val = execOperandValue op2 state
        let result = getBitwiseFunction core <| op1Val <| op2Val
        // Need to update CSPR here
        State.updateRegister dest result state
    | false ->
        state

let executeInstruction (state:State) (instruction:Instruction) =
        match instruction with
        | ArithmeticInstruction (operation, operands) ->
            executeArithmetic operation operands
        | ShiftInstruction (operation, operands) ->
            executeShift operation operands
        | CompareInstruction (operation, operands) ->
            executeCompare operation operands
        | BitwiseInstruction (operation, operands) ->
            executeBitwise operation operands
