module Execution

// open NewALU
// open Memory
// open Branch

type Instruction =
    private | ALUInst of ALU.ALUInstruction
            | MemInst of Memory.MemoryInstruction
            | BrInst of Branch.BranchInstruction

module Instruction =
    let constructSample () =
        ALUInst <| ALU.ALUInstruction.constructSample ()

    let execute state instruction =
        match instruction with
        | ALUInst ai -> ALU.ALUInstruction.execute state ai
        | MemInst mi -> Memory.MemoryInstruction.execute state mi
        | BrInst bi -> Branch.BranchInstruction.execute state bi
