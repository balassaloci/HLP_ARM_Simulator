module Branch

open Machine
open InstructionsCommonTypes
open CommonOperandFunctions
open CommonParserFunctions
open ErrorHandler

type Branch = | B | BL
type End = | END

type EndInstruction = {opcode: End; cond: ConditionSuffix}

type BranchInstruction = 
    {
        opcode: Branch;
        cond: ConditionSuffix;
        label: string;
    }

type ControlInstruction =
        | BInstr of BranchInstruction
        | EInstr of EndInstruction

module BranchParser =
    let parseLine (line:string) : ControlInstruction =
        if line.StartsWith("END") then
            let scode = getCond line.[3..]
            let instr : EndInstruction = {opcode=END; cond=scode}
            instr |> EInstr
        else
            let cleanLine = line |> decomment |> trimmer |> splitInstr
            let instrStr : string = fst cleanLine
            let paramStr : string = snd cleanLine
            //let splitOper = splitOperands paramStr

            let instr =
                match instrStr with
                | Prefix "BL" r ->
                    let scode = snd (getSCond r)
                    let combined : BranchInstruction = {opcode = BL;
                                                        cond = scode;
                                                        label = paramStr}

                    combined |> BInstr

                | Prefix "B" r ->
                    let scode = snd (getSCond r)
                    let combined : BranchInstruction = {opcode = B;
                                                        cond = scode;
                                                        label = paramStr}

                    combined |> BInstr

                | x ->  failc ("Unable to parse branch instruction: " + x)
                        
            instr

    
       
    
[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ControlInstruction =
    //let private branchLabel () = failwithf "Not implemented"
    open BranchParser

    let parse: string -> ControlInstruction =
        parseLine

    let private executeBranch (label:string) state = 
        let nextInstructionAddress = State.getLabelAddress label state
        State.updateSystemRegister PC nextInstructionAddress state

    let private executeBranchWithLink (label:string) state =
        let nextInstructionAddress = State.getLabelAddress label state
        let originalPCAddress = State.systemRegisterValue PC state
        state |> State.updateSystemRegister LR originalPCAddress 
            |> State.updateSystemRegister PC nextInstructionAddress

    let private executeBranchInstruction state (instr:BranchInstruction) = 
        let {opcode = core; cond = c; label = l} = instr;
        if conditionHolds state c then
            match core with
            | B -> executeBranch l state
            | BL -> executeBranchWithLink l state
        else
            state
    
    let private executeEndInstruction state (instr:EndInstruction) =
        let cond = instr.cond
        if conditionHolds state cond then
            State.endExecution state
        else state
    
    let execute state (instr: ControlInstruction) =
        match instr with
        |BInstr bi -> executeBranchInstruction state bi
        |EInstr ci -> executeEndInstruction state ci