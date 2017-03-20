module Branch

open Machine
open InstructionsCommonTypes
open CommonOperandFunctions
open CommonParserFunctions
open ErrorHandler

type Branch = | B | BL
type End = | END

type private EndInstruction = {opcode: End; cond: ConditionSuffix}

type private BranchInstruction = 
    {
        opcode: Branch;
        cond: ConditionSuffix;
        label: string;
    }

type ControlInstruction =
    private
        | BInstr of BranchInstruction
        | CInstr of EndInstruction

module BranchParser =
    let parseLine line : BranchInstruction Maybe =
        maybe {
            let cleanLine = line |> decomment |> trimmer |> splitInstr

            let instrStr : string = fst cleanLine
            let paramStr : string = snd cleanLine
            //let splitOper = splitOperands paramStr

            let! instr = match instrStr with
                            | Prefix "BL" r ->
                                let scode = snd (getSCond r)
                                let combined : BranchInstruction = {opcode = BL;
                                                                    cond = scode;
                                                                    label = paramStr}

                                Success combined

                            | Prefix "B" r ->
                                let scode = snd (getSCond r)
                                let combined : BranchInstruction = {opcode = B;
                                                                    cond = scode;
                                                                    label = paramStr}

                                Success combined
                            //| Prefix "B" r -> Error
                            | _ ->  Error <| (ParseError "Unable to parse branch instruction")
                        
            return instr

        }
       
    
[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ControlInstruction =
    //let private branchLabel () = failwithf "Not implemented"
    open BranchParser

    let parse: string -> BranchInstruction =
        fun x-> match parseLine x with
                | Success t -> t
                | Error x -> failwithf "%A" x

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
        |CInstr ci -> executeEndInstruction state ci