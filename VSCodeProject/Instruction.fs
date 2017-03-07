module Instruction

open Machine

type AddressExpression = | Label of string | Number of int

type ConditionSuffix = | EQ | NE | CS | HS | CC | LO | MI | PL | VS | VC | HI
                       | LS | GE | LT | GT | LE | AL

type SetBit = | UpdateStatus | IgnoreStatus

// For now, keep the separation, but do we need it in the future?
type Arithmetic = | ADD | SUB | ADC | SBC | RSC
type Shift = | LSL | LSR | ASR | ROR | RRX
type Compare = | CMP | CMN | TST | TEQ
type Bitwise = | AND | EOR | BIC | ORR

type Move = | MOV | MVN

type Branch = | B | BL

type MemorySingle = | LDR | STR
type MemoryMultiple = | STM | LDM
type MemoryAddress = | ADR

type StackDirection = | FA | FD | EA | ED | IB | IA | DB | DA
type ByteMode = | LoadByte | LoadWord


type ArithmeticOperation = Arithmetic * SetBit * ConditionSuffix
type BitwiseOperation = Bitwise * SetBit * ConditionSuffix
type ShiftOperation = Shift * SetBit * ConditionSuffix
type CompareOperation = Compare * ConditionSuffix
type MoveOperation = Move * SetBit * ConditionSuffix

type SingleRegisterMemoryOperation = MemorySingle * ByteMode * ConditionSuffix
type MultipleRegistersMemoryOperation = MemoryMultiple * StackDirection * ConditionSuffix
type LoadAddressOperation = MemoryAddress * SetBit * ConditionSuffix


type RegOperand = RegisterIndex
type MixedOperand = | Register of RegisterIndex | Literal of int
type ExecOperand = | MixedOp of MixedOperand
                   | ExprOp of RegOperand * Shift * MixedOperand


// ALU
type ArithmeticOperandsPattern = RegOperand * RegOperand * ExecOperand
type BitwiseOperandsPattern = ArithmeticOperandsPattern
// Need special case for RRX shift
type ShiftOperandsPattern = RegOperand * RegOperand * MixedOperand
type CompareOperandsPattern = RegOperand * ExecOperand

type MoveIOperandsPattern = RegOperand * ExecOperand

// LDR STR
type AddressingMethod = | Offset | PreIndexed | PostIndexed
//                            dest        source        [! stuff          OFFSET
type SingleRegisterMemoryOperandsPattern = RegOperand * RegOperand * AddressingMethod * ExecOperand
type MultipleRegistersMemoryOperandsPattern = RegOperand * RegOperand list
type LoadAddressOperands = RegOperand * AddressExpression

type ALUInstruction =
    | ArithmeticInstruction of ArithmeticOperation * ArithmeticOperandsPattern
    | ShiftInstruction of ShiftOperation * ShiftOperandsPattern
    | CompareInstruction of CompareOperation * CompareOperandsPattern
    | BitwiseInstruction of BitwiseOperation * BitwiseOperandsPattern

type DataInstruction =
    | MoveInstruction of MoveOperation * MoveIOperandsPattern

    | SingleMemoryInstruction of SingleRegisterMemoryOperation * SingleRegisterMemoryOperandsPattern
    | MultipleMemoryInstruction of MultipleRegistersMemoryOperation * MultipleRegistersMemoryOperandsPattern
    | LoadAddressInstruction of LoadAddressOperation * LoadAddressOperands

// Now only need to do branching, word declaration in memory and stop emulation