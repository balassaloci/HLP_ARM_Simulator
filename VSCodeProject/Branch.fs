module Branch

open Machine
open InstructionsCommonTypes

type BranchInstruction = {
        opcode: string;
        operands: string
    }

[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module BranchInstruction =
    let private branchLabel () = failwithf "Not implemented"

    let execute (state:State) (instr:BranchInstruction) = 
        failwithf "Not implemented"