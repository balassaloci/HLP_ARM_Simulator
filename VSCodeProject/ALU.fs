module ALU

open Machine
open Instruction
open Functions
open CommonOperandFunctions

let executeArithmetic (operation:ArithmeticOperation)
                      (operands:ArithmeticOperandsPattern)
                      (state:State) =
    let core, S, cond = operation
    match conditionHolds state cond with
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
    if conditionHolds state cond then
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
    if conditionHolds state cond then
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
    if conditionHolds state cond then
        let dest, op1, op2 = operands
        let op1Val = State.registerValue op1 state
        let op2Val = execOperandValue op2 state
        let result = getBitwiseFunction core <| op1Val <| op2Val
        // Need to update CSPR here
        State.updateRegister dest result state
    else
        state

