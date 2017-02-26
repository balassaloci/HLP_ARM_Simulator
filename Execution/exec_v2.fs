type RegisterIndex = | R0 | R1 | R2

// type APSR = {N:bool; Z:bool; C:bool; V:bool}
// let mutable statusRegister:APSR = {N=false;Z=false;C=false;V=false}
// let updateStatusRegister N Z C V =
//     {N=N; Z=Z; C=C;V=V}

// statusRegister <- updateStatusRegister true false false false

type ConditionSuffix = | EQ | NE | CS | HS | CC | LO | MI | PL | VS | VC | HI
                       | LS | GE | LT | GT | LE | AL | NoSuffix

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
type ExecOperand = | MixedOp of MixedOperand | ExprOp of Shift * MixedOperand


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


let operation1: ArithmeticOperation = ADD, UpdateStatus, EQ
let operation2: ShiftOperation = LSL, IgnoreStatus, NoSuffix

let operands1 = ArithmeticOperands <| (R0, R1, MixedOp <| Register R2)
let operands2 = ShiftOperands <| (R0, R1, Literal 3)

let instruction1 = ArithmeticInstruction (operation1, (R0, R1, MixedOp <| Register R2))
let instruction2 = ShiftInstruction (operation2, (R0, R1, Literal 3))

let executeArithmetic (operation:ArithmeticOperation) (operands:ArithmeticOperandsPattern) =
    match operation with
    | (op, S, cond) -> 1
    |> ignore

    let dest, op1, op2 = 
        match operands with
        | (dest, op1, op2) -> (dest, op1, op2)
    // Need to evaluate op2
    // type ExecOperand = | MixedOp of MixedOperand | ExprOp of Shift * MixedOperand
    // type MixedOperand = | Register of RegisterIndex | Literal of int
    1

let executeShift operation operands =
    match operation with
    | (op, S, cond) -> 1
    |> ignore
    1

let executeInstruction = function
    | ArithmeticInstruction(op, operands) -> executeArithmetic op operands
    | ShiftInstruction(op, operands) -> executeShift op operands
    | CompareInstruction(op, operands) -> 3
