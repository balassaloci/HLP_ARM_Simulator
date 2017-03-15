module NewExecute

open NewALU

type Instruction =
    private | ALUInst of NewALU.ALUInstruction1

module Instruction =
    let execute state instruction =
        match instruction with
        | ALUInst ai -> NewALU.ALUInstruction1.execute state ai
