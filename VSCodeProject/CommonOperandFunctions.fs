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
let execOperandValue op (state:State) =
    match op with
    | MixedOp mop -> int64 <| mixedOperandValue mop state, 0
    | ExprOp (op2, shift, expr) ->
        let op2Val = registerOperandValue op2 state
        let shiftVal = mixedOperandValue expr state
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

