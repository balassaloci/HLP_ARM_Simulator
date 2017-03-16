module Memory

open Machine
open Instruction
open CommonOperandFunctions

//move to commonOperandFunctions
let addressExpressionValue (state: State) (expression: AddressExpression) =
    match expression with
    | Label l -> State.getLabelAddress l state
    | Number n -> n

// let getSingleRegisterMemoryFunction = function
//     | LDR -> loadSingleRegister
//     | STR -> storeSingleRegister


let executeMove (operation: MoveOperation)
                (operands: MoveIOperandsPattern)
                (state: State)  =
    let core, s, cond = operation
    match conditionHolds state cond with
    | true -> 
        let dest, op1 = operands
        let op1Val = execOperandValue op1 state
        //CSPR
        State.updateRegister dest op1Val state
    | false -> state
            
let executeSingleRegisterMemory (operation: SingleRegisterMemoryOperation)
                                (operands: SingleRegisterMemoryOperandsPattern)
                                (state: State) =
    let core, b, cond = operation
    match conditionHolds state cond with
    | true -> state
    | false -> state

let executeMultipleRegisterMemory (operation: MultipleRegistersMemoryOperation)
                                  (operands: MultipleRegistersMemoryOperandsPattern)
                                  (state: State) = 
    let core, dir, cond = operation
    match conditionHolds state cond with
    | true -> state
    | false -> state

let executeLoadAddress (operation: LoadAddressOperation)
                       (operands: LoadAddressOperands)
                       (state: State) = 
    let core, s, cond = operation
    match conditionHolds state cond with
    | true -> 
        let dest, exp = operands
        let adr = addressExpressionValue state exp
        State.updateRegister dest adr state //does not work for system registers
    | flase -> state


//-----------------------------Branch Instructions

type BranchOperation = Branch * ConditionSuffix
type ControlFlowInstruction = |BranchInstruction of BranchOperation * string

let branch (label:string) state = 
    let nextInstructionAddress = State.getLabelAddress label state
    State.updateSystemRegister PC nextInstructionAddress state

let branchWithLink (label:string) state =
    let nextInstructionAddress = State.getLabelAddress label state
    let originalPCAddress = State.systemRegisterValue PC state
    state |> State.updateSystemRegister LR originalPCAddress 
          |> State.updateSystemRegister PC nextInstructionAddress

let getBranchFunction = function
    | B -> branch
    | BL -> branchWithLink
let executeBranch (operation: BranchOperation) 
                  (label: string)
                  (state: State) = 
    let core, cond = operation
    match conditionHolds state cond with
    | true -> getBranchFunction core <| label <| state
    | false -> state
    