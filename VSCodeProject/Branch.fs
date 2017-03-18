module Branch

open Machine
open InstructionsCommonTypes
open CommonOperandFunctions

type Branch = B | BL

type BranchInstruction = 
    private{
        opcode: Branch;
        cond: ConditionSuffix;
        label: string;
    }

[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module BranchInstruction =
    //let private branchLabel () = failwithf "Not implemented"

    let private executebranch (label:string) state = 
        let nextInstructionAddress = State.getLabelAddress label state
        State.updateSystemRegister PC nextInstructionAddress state

    let private executebranchWithLink (label:string) state =
        let nextInstructionAddress = State.getLabelAddress label state
        let originalPCAddress = State.systemRegisterValue PC state
        state |> State.updateSystemRegister LR originalPCAddress 
            |> State.updateSystemRegister PC nextInstructionAddress

    let execute (state:State) (instr:BranchInstruction) = 
        let {opcode = core; cond = c; label = l} = instr;
        if conditionHolds state c then
            match core with
            | B -> executebranch l state
            | BL -> executebranchWithLink l state
        else
            state