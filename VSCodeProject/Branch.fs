module Branch

open Machine
open InstructionsCommonTypes
open CommonOperandFunctions
open CommonParserFunctions
open ErrorHandler

type Branch = B | BL

type BranchInstruction = 
    private{
        opcode: Branch;
        cond: ConditionSuffix;
        label: string;
    }

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
module BranchInstruction =
    //let private branchLabel () = failwithf "Not implemented"
    open BranchParser

    let parse: string -> BranchInstruction =
        fun x-> match parseLine x with
                | Success t -> t
                | Error x -> failwithf "%A" x

    let private executebranch (label:string) state = 
        let nextInstructionAddress = State.getLabelAddress label state
        State.updateSystemRegister PC nextInstructionAddress state

    let private executebranchWithLink (label:string) state =
        let nextInstructionAddress = State.getLabelAddress label state
        let originalPCAddress = State.systemRegisterValue PC state
        state |> State.updateSystemRegister LR originalPCAddress 
            |> State.updateSystemRegister PC nextInstructionAddress

    let execute state (instr:BranchInstruction) = 
        let {opcode = core; cond = c; label = l} = instr;
        if conditionHolds state c then
            match core with
            | B -> executebranch l state
            | BL -> executebranchWithLink l state
        else
            state