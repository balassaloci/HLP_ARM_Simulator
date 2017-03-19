module InstructionsCommonTypes



type ConditionSuffix = | EQ | NE | CS | HS | CC | LO | MI | PL | VS | VC | HI
                       | LS | GE | LT | GT | LE | AL

type SetBit = | UpdateStatus | IgnoreStatus

type Arithmetic = | ADD | SUB | ADC | SBC | RSC
type Shift = | LSL | LSR | ASR | ROR | RRX
type Compare = | CMP | CMN | TST | TEQ
type Bitwise = | AND | EOR | BIC | ORR

type RegisterIndex = 
    | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
    | R9 | R10 |R11 | R12 | R13 | LR | PC

type RegOperand = RegisterIndex

type MixedOperand = | Register of RegisterIndex | Literal of int

// TODO: ExecOperand is not tight enough!
/// MixedOp constant must be a number creatable by rotating 8-bit number
/// ExprOp MixedOperand constant must be up to 32 or 31 [depends]
type ExecOperand = | MixedOp of MixedOperand
                   | ExprOp of RegOperand * Shift * MixedOperand

type BitwiseNumber = {body:int; carry:int}

// type Instruction =
//     | ALUInst of ALU.ALUInstruction
//     | MemInst of Memory.MemoryInstruction
//     | BrInst of Branch.BranchInstruction

type 'T foo = | A of 'T

// let bar = A Instruction