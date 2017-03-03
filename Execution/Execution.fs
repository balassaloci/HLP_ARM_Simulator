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

let operandValue state = function
    | Reg x -> registerOperandValue x state
    | Mix x -> mixedOperandValue x state
    | Exe x -> execOperandValue x state

let getOperands = function
    | ArithmeticOperands (a,b,c) -> [|Reg a; Reg b; Exe c|]
    | BitwiseOperands (a, b, c) -> [|Reg a; Reg b; Exe c|]
    | ShiftOperands (a, b, c) -> [|Reg a; Reg b; Mix c|]
    | CompareOperands (a, b) -> [|Reg a; Exe b|]

let getFunction = function
    | AR x -> getArithmeticFunction x        
    | SH x -> getShiftFunction x
    | CO x -> getCompareFunction x
    | BI x -> getBitwiseFunction x

let getOperation = function
    | IAR (c,s,cond) -> (AR c,s,cond)
    | ISH (c,s,cond) -> (SH c,s,cond)
    | ICO (c, cond) -> (CO c, IgnoreStatus, cond)
    | IBI (c,s,cond) -> (BI c,s,cond)

let executeGeneric operation operands state =
    let core, S, cond = getOperation operation
    if conditionHolds cond then
        let operandArray = getOperands operands
        let dest = match operandArray.[0] with | Reg x -> x

        let operandValues = Array.map (operandValue state) operandArray
        let foo = getFunction core
        let result = match operandValues with
                     | [|a;b;c|] -> foo b c
                     | [|a;b|] -> foo a b
                     | _ -> 0
        // TODO: update CSPR
        State.updateRegister dest result state
    else
        state

let executeInstruction (state:State) (instruction:Instruction) =
    let operands, operation = 
        match instruction with
        | ArithmeticInstruction (operation, operands) ->
            ArithmeticOperands operands ,IAR operation
        | ShiftInstruction (operation, operands) ->
            ShiftOperands operands ,ISH operation
        | CompareInstruction (operation, operands) ->
            CompareOperands operands ,ICO operation
        | BitwiseInstruction (operation, operands) ->
            BitwiseOperands operands, IBI operation

    executeGeneric operation operands state

let executeInstructionGeneric (state:State) (instruction:Instruction) =

// let operation1: ArithmeticOperation = ADD, UpdateStatus, AL
// let instruction = ArithmeticInstruction (operation1, (R0, R1, MixedOp <| Literal 55))

// 0
// let newState = executeInstruction initialState instruction

