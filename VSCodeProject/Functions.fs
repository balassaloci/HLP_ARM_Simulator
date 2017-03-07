module Functions

open Machine
open Instruction

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
let arithmeticShiftRight op1 op2 = leftshift op1 op2
let rotateRight op1 op2 = leftshift op1 op2

let add a b = a + b
let addCarry a b =
    // let c = if statusRegister.C then 1 else 0
    let c = 0
    add a c |> add b

let subtract a b = a - b
let subtractCarry a b =
    // let c = if statusRegister.C then 0 else 1
    let c = 0
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
    | ASR | ROR | RRX -> leftshift

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

