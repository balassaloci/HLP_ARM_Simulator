module State

open Machine
open Execution

let initialMemory = Map.empty<uint32,int>

let regList = 
    [(R0,0); (R1,0); (R2,0); (R3,0); (R4,0); (R5,0); (R6,0); (R7,0); 
    (R8,0); (R9,0); (R10,0); (R12,0); (R13,0); (LR,0); (PC,0); (CSPR,0)]
    
let initialRegisters = Map.ofList regList


let state = {Register = initialRegisters;Memory = initialMemory}

// list of instructions with type Instruction (from exec file)

let rec runProgram instructionsList (state: State) =
    match instructionsList with
    | h :: t -> runProgram t (executeInstruction h state)
    | _ -> state
