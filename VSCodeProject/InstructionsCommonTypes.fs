module InstructionsCommonTypes

open Machine
type ConditionSuffix = | EQ | NE | CS | HS | CC | LO | MI | PL | VS | VC | HI
                       | LS | GE | LT | GT | LE | AL

type SetBit = | UpdateStatus | IgnoreStatus

type Arithmetic = | ADD | SUB | ADC | SBC | RSC
type Shift = | LSL | LSR | ASR | ROR | RRX
type Compare = | CMP | CMN | TST | TEQ
type Bitwise = | AND | EOR | BIC | ORR


type RegOperand = RegisterIndex

type MixedOperand = | Register of RegisterIndex | Literal of int

// TODO: ExecOperand is not tight enough!
/// MixedOp constant must be a number creatable by rotating 8-bit number
/// ExprOp MixedOperand constant must be up to 32 or 31 [depends]
type ExecOperand = | MixedOp of MixedOperand
                   | ExprOp of RegOperand * Shift * MixedOperand

type BitwiseNumber = {body:int; carry:int}
