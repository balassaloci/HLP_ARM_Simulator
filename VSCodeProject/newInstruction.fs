open Machine
open NewALU

type Instruction1 =
    private | ALUInst of NewALU.ALUInstruction1

module Instruction1 =
    let execute state instruction =
        match instruction with
        | ALUInst ai -> NewALU.ALUInstruction1.execute state ai