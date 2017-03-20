module Execution

open Machine
open InstructionsCommonTypes

type Instruction =
    | ALUInst of ALU.ALUInstruction
    | MemInst of Memory.MemoryInstruction
    | BrInst of Branch.BranchInstruction



[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Instruction =
    let constructSample () =
        ALUInst <| ALU.ALUInstruction.constructSample ()

    let private execute state instruction =
        match instruction with
        | ALUInst ai -> ALU.ALUInstruction.execute state ai
        | MemInst mi -> Memory.MemoryInstruction.execute state mi
        | BrInst bi -> Branch.BranchInstruction.execute state bi

    let run state =
        let pcAddress = State.registerValue PC state
        let inst = State.getInstruction pcAddress state
        let newState = State.incrementPC state
        execute newState inst
    