module CommonOperandFunctions

open Machine
open InstructionsCommonTypes
open Functions

/// Fetches the numeric value stored in given register
let registerOperandValue op state =
    State.registerValue op state

/// Extracts numeric value from mixed operand
let mixedOperandValue op state =
    match op with
    | Register R -> State.registerValue R state
    | Literal x -> x

/// Either extracts numeric value from operand or the value
/// after shifting the operand.
let execOperandValue op state =
    /// Returns true if there exists an 8 LSB bit rotation of this number
    let is8bitRotated x =
        let shifts = [1..32]
        let rotateRight' x r = (rotateRight x r).body
        /// Returns true if given rotation fits in 8LSBs
        let checkRotation x =
            let bitMask8 = (~~~) <| 0xFF
            x |> (&&&) bitMask8 |> (=) 0
        List.map (rotateRight' x >> checkRotation) shifts
        |> List.reduce (||)
    
    match op with
    | MixedOp mop ->
        let value = int64 <| mixedOperandValue mop state
        if is8bitRotated <| int value then
            value, 0
        else
            failwith "Constant must created by rotating 8bit number"

    | ExprOp (op2, shift, expr) ->
        let op2Val = registerOperandValue op2 state
        let shiftVal = mixedOperandValue expr state
        match shift with
        | ASR | LSR when shiftVal > 32 ->
                           failwith "Shift must be less that 32"
        | LSL | ROR when shiftVal > 31 ->
                           failwith "Shift value must be less than 31"
        | _ -> ()

        let {body=num; carry=c} = applyShiftFunction shift 0 op2Val shiftVal
        int64 num, c

/// Returns true if the given condition code is satisfied
let conditionHolds state cond = 
    let getBool state statusBit =
        State.statusBitValue statusBit state
    
    let getStateFlag = getBool state

    match cond with
    | AL -> true
    | EQ -> getStateFlag Z
    | NE -> not <| getStateFlag Z
    | CS | HS -> getStateFlag C
    | CC | LO -> not <| getStateFlag C
    | MI -> getStateFlag N
    | PL -> not <| getStateFlag N
    | VS -> getStateFlag V
    | VC -> not <| getStateFlag V
    | HI -> getStateFlag C && not <| getStateFlag Z
    | LS -> not <| getStateFlag C && getStateFlag Z
    | GE -> getStateFlag N = getStateFlag V
    | LT -> getStateFlag N <> getStateFlag V
    | GT -> (getStateFlag N = getStateFlag V) && not <| getStateFlag Z
    | LE -> (getStateFlag N <> getStateFlag V) && getStateFlag Z

