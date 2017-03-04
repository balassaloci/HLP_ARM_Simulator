module Instruction

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
type Shift = | LSL | LSR | ASR | ROR | RRX
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


type IOperand = | Reg of RegOperand | Mix of MixedOperand | Exe of ExecOperand
type IOperands = | ArithmeticOperands of ArithmeticOperandsPattern
                 | ShiftOperands of ShiftOperandsPattern
                 | CompareOperands of CompareOperandsPattern
                 | BitwiseOperands of BitwiseOperandsPattern

type OperationCore = | AR of Arithmetic | SH of Shift
                     | CO of Compare | BI of Bitwise
type OperationType = | IAR of ArithmeticOperation
                     | ISH of ShiftOperation
                     | ICO of CompareOperation
                     | IBI of BitwiseOperation
type Instruction =
    | ArithmeticInstruction of ArithmeticOperation * ArithmeticOperandsPattern
    | ShiftInstruction of ShiftOperation * ShiftOperandsPattern
    | CompareInstruction of CompareOperation * CompareOperandsPattern
    | BitwiseInstruction of BitwiseOperation * BitwiseOperandsPattern

