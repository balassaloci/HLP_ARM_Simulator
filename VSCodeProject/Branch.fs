module Branch

open Machine
open NewInstruction

type BranchInstruction =
    private {
        opcode: string
        operands: string
    }

module BranchInstruction =
    let private branchLabel () = failwithf "Not implemented"

    let execute (state:State) (instr:BranchInstruction) = 
        failwithf "Not implemented"