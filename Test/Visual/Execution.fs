// namespace Visual
// module Execution
module Execution
open Machine

// type RegisterIndex = | R0 | R1 | R2

type APSR = {N:bool; Z:bool; C:bool; V:bool}
let mutable statusRegister:APSR = {N=false;Z=false;C=false;V=false}
let updateStatusRegister N Z C V =
    statusRegister <- {N=N; Z=Z; C=C;V=V}

// statusRegister <- updateStatusRegister true false false false

type ConditionSuffix = | EQ | NE | CS | HS | CC | LO | MI | PL | VS | VC | HI
                        | LS | GE | LT | GT | LE | AL

type SetBit = | UpdateStatus | IgnoreStatus

// For now, keep the separation, but do we need it in the future?
type Arithmetic = | ADD | SUB
type Shift = | LSL
type Compare = | CMP | CMN | TST | TEQ

// type OperationCore = | Arithmetic of ArithmeticOperation | Shift of ShiftOperation
// The following is simply conveninet in "Instruction" type definition;
// Issue: 'a can even be int, but is this a problem? Shouldn't be
// 'a is in place of OperationCore
type 'a IOperation = 'a * SetBit * ConditionSuffix

type ArithmeticOperation = Arithmetic * SetBit * ConditionSuffix
type ShiftOperation = Shift * SetBit * ConditionSuffix
type CompareOperation = Compare * ConditionSuffix

// Need to decide how to handle the expression for op2
type RegOperand = RegisterIndex
type MixedOperand = | Register of RegisterIndex | Literal of int
type ExecOperand = | MixedOp of MixedOperand | ExprOp of RegOperand * Shift * MixedOperand


// ADD{S}{cond} dest, op1, op2 {, SHIFT_op #expression}
type ArithmeticOperandsPattern = RegOperand * RegOperand * ExecOperand
// Need special case for RRX
// LSL{S}{cond} dest, op1, op2
type ShiftOperandsPattern = RegOperand * RegOperand * MixedOperand
// CMP{cond} op1, op2 {, SHIFT_op #expression}
type CompareOperandsPattern = RegOperand * MixedOperand


type IOperands = | ArithmeticOperands of ArithmeticOperandsPattern
                | ShiftOperands of ShiftOperandsPattern
// type Instruction = IOperation * IOperands

type Instruction =
    | ArithmeticInstruction of ArithmeticOperation * ArithmeticOperandsPattern
    | ShiftInstruction of ShiftOperation * ShiftOperandsPattern
    | CompareInstruction of CompareOperation * CompareOperandsPattern

// let mutable registers = Map<RegisterIndex, int> []
// type State = Map<RegisterIndex, int>
// let initialState:State = Map<RegisterIndex, int> [(R1, 5)]

// let state:State = 

let leftshift (op1:int) (op2:int) = op1 <<< op2


// let fetchValue (R:RegisterIndex) = initialState.[R]

// let writeValue (R:RegisterIndex) (value:int) =
//     (fun (s : State) -> Map.add R value s) |> ignore
    // registers <- Map.add R value registers

// let operandValue = function
//     | Register x -> fetchValue x
//     | Literal x -> x
//     | MixedOp mop -> mixedOperandValue mop

let mixedOperandValue op state =
    match op with
    | Register R -> State.registerValue R state
    | Literal x -> x

let execOperandValue op (state:State) =
    match op with
    | MixedOp mop -> mixedOperandValue mop state
    // | ->
    | ExprOp (op2, shift, expr) -> leftshift (State.registerValue op2 state) (mixedOperandValue expr state)
        // let x = State.registerValue op2 state
        // x
        // leftshift (State.registerValue op2 state) (mixedOperandValue expr state)

// let state0:State = Map.ofList []
// let res = writeValue R0 5
//           >> writeValue R1 6
//           >> writeValue R2 1

// printf "%A\n" (res state0)
// let operation1: ArithmeticOperation = ADD, UpdateStatus, EQ
// let operation2: ShiftOperation = LSL, IgnoreStatus, NoSuffix

// let operands1 = ArithmeticOperands <| (R0, R1, MixedOp <| Register R2)
// let operands2 = ShiftOperands <| (R0, R1, Literal 3)

// let instruction1 = ArithmeticInstruction (operation1, (R0, R1, MixedOp <| Register R2))
// let instruction2 = ShiftInstruction (operation2, (R0, R1, Literal 3))

let conditionHolds = function
    | AL -> true
    | EQ -> statusRegister.Z
    | NE -> statusRegister.Z
    | CS | HS -> statusRegister.C
    | _ -> false

// let (|ADD_CSPR|_|) x =
//     let N = x < 0
//     let Z = x = 0
//     let C = x > (1<<<32)
//     let V = false
//     (N, Z, C, V)
// let (|N|Z|C|V|) x =
    
//     if x < 0 then true else false


// let updateStatus result =
//     match result with
//     | 
    
let add a b = a + b
let subtract a b = a - b

let getFunction = function
    | ADD -> add
    | SUB -> subtract

// let lookup = Map<Arithmetic, int -> int -> int>
// let lookup = Map.ofList [(ADD, add0), (SUB, subtract)]
let executeArithmetic (operation:ArithmeticOperation)
                      (operands:ArithmeticOperandsPattern)
                      (state:State) =
    let core, S, cond = operation
    match conditionHolds cond with
    | true ->
        let dest, op1, op2 = operands
        let op1Val, op2Val = State.registerValue op1 state, execOperandValue op2 state
        let result = getFunction core <| op1Val <| op2Val
        // result
        State.updateRegister dest result state

        // Map.add dest result state
    | false ->
        state
    // let dest, op1, op2 = 
    //     match operands with
    //     | (dest, op1, op2) -> (dest, op1, op2)
    // Need to evaluate op2
    // type ExecOperand = | MixedOp of MixedOperand | ExprOp of Shift * MixedOperand
    // type MixedOperand = | Register of RegisterIndex | Literal of int

// let executeShift operation operands =
//     match operation with
//     | (op, S, cond) -> 1
//     |> ignore
//     1

let executeInstruction (state:State) (instruction:Instruction) =
    match instruction with
    | ShiftInstruction (operation, operands) -> state
    | ArithmeticInstruction (operation, operands) -> executeArithmetic operation operands state
    | _ -> state

// let operation1: ArithmeticOperation = ADD, UpdateStatus, AL
// let instruction = ArithmeticInstruction (operation1, (R0, R1, MixedOp <| Literal 55))

// let newState = executeInstruction initialState instruction

