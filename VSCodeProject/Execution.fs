module Execution

open Machine
open Instruction
// open Functions
open ALU
open Memory

// let checkCarry (add:bool) (result:int) =
//     if add && result >= (1<<<32) then
//         1
//     elif not <| add && result >= 0 then
//         0
//     else
//         0

// let checkOverflow () =
//     0

let executeALUInstruction (state:State) = function 
    | ArithmeticInstruction (operation, operands) ->
        executeArithmetic operation operands
    | ShiftInstruction (operation, operands) ->
        executeShift operation operands
    | CompareInstruction (operation, operands) ->
        executeCompare operation operands
    | BitwiseInstruction (operation, operands) ->
        executeBitwise operation operands

let executeDataInstruction (state:State) = function
    | MoveInstruction (operation, operands) -> state
    | SingleMemoryInstruction (operation, operands) -> state
    | MultipleMemoryInstruction (operation, operands) -> state
    | LoadAddressInstruction (operation, operands) -> state
