module Execution

open Machine
open InstructionsCommonTypes
open ALU
open Memory
open Branch
open CommonParserFunctions

type Instruction =
    | ALUInst of ALU.ALUInstruction
    | MemInst of Memory.MemoryInstruction
    | BrInst of Branch.BranchInstruction

let parseLine (instrS: string) :  (string Option * Instruction) =
    let parseInstruction (instrS: string) : Instruction =
        if instrS.Length > 2 then
            match instrS.[..2] with
            | "ADD" | "SUB" | "ADC" | "SBC" | "RSC"
                | "LSL" | "LSR" | "ASR" | "ROR" | "RRX"
                | "CMP" | "CMN" | "TST" | "TEQ" | "AND"
                | "EOR" | "BIC" | "ORR" -> ALUInst <| ALUInstruction.parse instrS
            | "MOV" | "MVN" | "LDR" | "STR" | "LDM"
                | "STM" | "ADR" -> MemInst <| MemoryInstruction.parse instrS
        else
            match instrS with
            | Prefix "B" _ -> BrInst <| BranchInstruction.parse instrS
            | _ -> failwithf "Unable to parse instruction"
    try
        None, parseInstruction (instrS)
    with
    | _ -> match splitInstr instrS with
           | l, i -> Some (l), parseInstruction i

let parseAll (txt: string) =
    1



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
    