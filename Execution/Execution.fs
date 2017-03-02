module Execution
open Machine


type APSR = {N:bool; Z:bool; C:bool; V:bool}
let mutable statusRegister:APSR = {N=false;Z=false;C=false;V=false}
let updateStatusRegister N Z C V =
    statusRegister <- {N=N; Z=Z; C=C;V=V}


type ConditionSuffix = | EQ | NE | CS | HS | CC | LO | MI | PL | VS | VC | HI
                       | LS | GE | LT | GT | LE | AL

type SetBit = | UpdateStatus | IgnoreStatus

// For now, keep the separation, but do we need it in the future?
type Arithmetic = | ADD | SUB | ADC | SBC | RSC
type Shift = | LSL | LSR | ASR | ROR
type Compare = | CMP | CMN | TST | TEQ
type Bitwise = | AND | EOR | BIC | ORR

type ArithmeticOperation = Arithmetic * SetBit * ConditionSuffix
type BitwiseOperation = Bitwise * SetBit * ConditionSuffix
type ShiftOperation = Shift * SetBit * ConditionSuffix
type CompareOperation = Compare * ConditionSuffix

type RegOperand = RegisterIndex
type MixedOperand = | Register of RegisterIndex | Literal of int
type ExecOperand = | MixedOp of MixedOperand
                   | ExprOp of RegOperand * Shift * MixedOperand


type ArithmeticOperandsPattern = RegOperand * RegOperand * ExecOperand
type BitwiseOperandsPattern = ArithmeticOperandsPattern
// Need special case for RRX shift
type ShiftOperandsPattern = RegOperand * RegOperand * MixedOperand
type CompareOperandsPattern = RegOperand * ExecOperand


type IOperands = | ArithmeticOperands of ArithmeticOperandsPattern
                 | ShiftOperands of ShiftOperandsPattern
                 | CompareOperands of CompareOperandsPattern
                 | BitwiseOperands of BitwiseOperandsPattern

type Instruction =
    | ArithmeticInstruction of ArithmeticOperation * ArithmeticOperandsPattern
    | ShiftInstruction of ShiftOperation * ShiftOperandsPattern
    | CompareInstruction of CompareOperation * CompareOperandsPattern
    | BitwiseInstruction of BitwiseOperation * BitwiseOperandsPattern

let bitwiseAnd op1 op2 =
    op1 &&& op2
let bitwiseOr op1 op2 =
    op1 ||| op2
let bitwiseXor op1 op2 =
    op1 ^^^ op2
let bitwiseClear op1 op2 =
    bitwiseAnd op1 ~~~op2

let leftshift op1 op2 = op1 <<< op2
let rightshift op1 op2 = op1 >>> op2

let add a b = a + b
let addCarry a b =
    let c = if statusRegister.C then 1 else 0
    add a c |> add b

let subtract a b = a - b
let subtractCarry a b =
    let c = if statusRegister.C then 0 else 1
    subtract a c |> subtract b

let reverseSubtract a b =
    subtract b a
let reverseSubtractCarry a b =
    subtractCarry b a

let compare op1 op2 = subtract op1 op2
let compareNegated op1 op2 = add op1 op2
let compareTest op1 op2 = bitwiseAnd op1 op2
let compareTeq op1 op2 = bitwiseXor op1 op2

let getShiftFunction = function
    | LSL -> leftshift
    | LSR -> rightshift
    | ASR | ROR -> leftshift

let getArithmeticFunction = function
    | ADD -> add
    | SUB -> subtract
    | ADC -> addCarry
    | SBC -> subtractCarry
    | RSC -> reverseSubtractCarry

let getBitwiseFunction = function
    | AND -> bitwiseAnd
    | EOR -> bitwiseXor
    | BIC -> bitwiseClear
    | ORR -> bitwiseOr

let getCompareFunction = function
    | CMP -> compare
    | CMN -> compareNegated
    | TST -> compareTest
    | TEQ -> compareTeq


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
        executeArithmetic operation operands state
    | ShiftInstruction (operation, operands) ->
        executeShift operation operands state
    | CompareInstruction (operation, operands) ->
        executeCompare operation operands state
    | BitwiseInstruction (operation, operands) ->
        executeBitwise operation operands state

// let operation1: ArithmeticOperation = ADD, UpdateStatus, AL
// let instruction = ArithmeticInstruction (operation1, (R0, R1, MixedOp <| Literal 55))

// let newState = executeInstruction initialState instruction

