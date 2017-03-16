module Memory

open Machine
open InstructionsCommonTypes
open CommonOperandFunctions

type MemoryInstruction =
    private {
        opcode: string
        operands: string list
    }

[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MemoryInstruction =
    let private executeMove () = None
    let private executeSingleRegisterMemoryInstruction () = None
    let private executeMultipleRegisterMemoryInstruction () = None
    let private executeLoadAddressInstruction () = None

    let execute (state:State) (instr:MemoryInstruction) =
        failwithf "Not implemented"