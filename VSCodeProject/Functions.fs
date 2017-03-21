module Functions

open Machine
open InstructionsCommonTypes

let bitwiseAnd op1 op2 =
    op1 &&& op2
let bitwiseOr op1 op2 =
    op1 ||| op2
let bitwiseXor op1 op2 =
    op1 ^^^ op2
let bitwiseClear op1 op2 =
    bitwiseAnd op1 ~~~op2

let leftshift (op1:int) op2 = 
    let tmpVal = op1 <<< op2-1
    let carry = (tmpVal >>> 31) &&& 1
    {body=tmpVal <<< 1; carry=carry}

let rightshift (op1:int) op2 =
    let tmpVal = int <| uint32 op1 >>> op2-1
    let carry = tmpVal &&& 1
    {body=tmpVal >>> 1; carry=carry}


let arithmeticShiftRight (op1:int) op2 =
    if op2 > 0 then
        let tmpVal = op1 >>> op2-1
        let carry = tmpVal &&& 1
        {body=tmpVal >>> 1; carry=carry}
    else
        {body=op1; carry=0}

// The carry seems to be the same as MSB bit
let rotateRight (op1:int) op2 =
    let mask = -1 + (int <| 2.0 ** float op2)
    let rotatedBits = uint32 (op1 &&& mask) <<< 32 - op2
    
    let numberBody = rotatedBits ||| (uint32 op1 >>> op2)
                     |> int
    let carry = (numberBody >>> 31) &&& 1
    {body=numberBody; carry=carry}

let rotateRightExtend body carry0 =
    let carry = body &&& 1

    let tmpVal = uint32 body >>> 1
    let resultBody = carry0 <<< 31 ||| int tmpVal
    {body=resultBody; carry=carry}

let add (c:int64) (a:int64) (b:int64) =
    // Remove 32 MSBs
    let a' = int64 <| uint32 a
    let b' = int64 <| uint32 b
    c + a' + b'

let subtract (c:int64) a b =
    let a' = int64 <| uint32 a
    let b' = int64 <| uint32 b
    a' - b' - c

let compare op1 op2 = subtract 0L op1 op2
let compareNegated op1 op2 = add 0L op1 op2
let compareTest op1 op2 = bitwiseAnd op1 op2
let compareTeq op1 op2 = bitwiseXor op1 op2

let getShiftFunction = function
    | LSL -> leftshift
    | LSR -> rightshift
    | ASR -> arithmeticShiftRight
    | ROR -> rotateRight
    | RRX -> rotateRightExtend

let applyArithmeticFunction core carry op1 op2 =
    let c = match core with
            | ADC | SBC | RSC -> carry
            | _ -> 0L
    match core with
    | ADD | ADC -> add c op1 op2
    | SUB -> subtract 0L op1 op2
    | SBC -> subtract (1L-c) op1 op2
    | RSC -> subtract (1L-c) op2 op1

let applyShiftFunction core carry op1 op2 =
    let c = if core = RRX then
                carry
            else 0
    match core with
    | LSL -> leftshift op1 op2
    | LSR -> rightshift op1 op2
    | ASR -> arithmeticShiftRight op1 op2
    | ROR -> rotateRight op1 op2
    | RRX -> rotateRightExtend op1 carry


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

let checkArithmeticCarry result addition state =
    let flag =
        if addition then
            result >= (1L <<< 32)
        else
            result >= 0L

    State.updateStatusBit C flag state

/// Compute the overflow bit for arithmetic on 64-bit numbers
let checkArithmeticOverflow result state =
    let bitSet n x = (x >>> n) &&& 1L = 1L
    let flag =  not <| bitSet 31 result = bitSet 63 result
    State.updateStatusBit V flag state

let checkNegative result state =
    let flag = result < 0L
    State.updateStatusBit N flag state

let checkZero result state =
    let flag = result = 0L
    State.updateStatusBit Z flag state
    
let updateArithmeticCSPR state result addition =
    let result1 = int64 <| int32 result
    state
    |> checkZero result1
    |> checkNegative result1
    |> checkArithmeticCarry result1 addition
    |> checkArithmeticOverflow result1