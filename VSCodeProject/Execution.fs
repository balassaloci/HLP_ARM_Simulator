module Execution

open Machine
open InstructionsCommonTypes
open ALU
open Memory
open Branch
open Other
open CommonParserFunctions

type Instruction =
    | ALUInst of ALU.ALUInstruction
    | MemInst of Memory.MemoryInstruction
    | BrInst of Branch.BranchInstruction
    | OInstr of Other.OtherInstruction

module ExecuteParser =
    let private parseLine (instrS: string) :  (string Option * Instruction) =
        let parseInstruction (instrS: string) : Instruction =
            if instrS.Length > 2 then
                match instrS.[..2] with
                | "ADD" | "SUB" | "ADC" | "SBC" | "RSC"
                    | "LSL" | "LSR" | "ASR" | "ROR" | "RRX"
                    | "CMP" | "CMN" | "TST" | "TEQ" | "AND"
                    | "EOR" | "BIC" | "ORR" -> ALUInst <| ALUInstruction.parse instrS
                | "MOV" | "MVN" | "LDR" | "STR" | "LDM"
                    | "STM" | "ADR" -> MemInst <| MemoryInstruction.parse instrS
                | "DCD" | "DCB" | "EQU" | "FILL" -> OInstr <| OtherInstruction.parse instrS
                | _ -> failwithf "Unable to parse instruction"
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
        let lines = txt.Split([|'\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.toList

        let rec matchLines lines (memInstr: Instruction array)
                                 (labels:Map<string, int>)
                                 (firstInstr: Instruction array) =
            match lines with
            | x::xn -> 
                let parsed = parseLine x
                let labels' =
                   match fst parsed with
                   | Some l -> labels.Add(l, memInstr.Length * 4)
                   | _ -> labels
                let newInstrs =
                    match snd parsed with
                    | OInstr i -> memInstr, (Array.append firstInstr [|snd parsed|])
                    | _ -> (Array.append memInstr [|snd parsed|]), firstInstr

                let memInstr' = fst newInstrs
                let firstInstr' = snd newInstrs

                matchLines xn memInstr' labels' firstInstr'
            | [] -> memInstr, labels, firstInstr

        let emptyMap : Map<string, int>= Map.empty
        let emptyIArr = Array.empty

        matchLines lines emptyIArr emptyMap emptyIArr

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
        | _ -> state //should never matched

    let private executeSpecialInstr instrList state =
        let executeSpecialOnce instr state =
            match instr with
            | OInstr oi -> Other.OtherInstruction.execute state oi
            | _ -> state //should never match
        let rec executeSpecialAll iList state =
            match iList with
            | h :: t ->
                state
                |> executeSpecialOnce h
                |> executeSpecialAll t
            | [] -> state
        executeSpecialAll instrList state

    let prepareState (text:string) =
        let instructions,labels,specialInstr = ExecuteParser.parseAll text
        let specialInstrList = List.ofArray specialInstr
        State.makeInitialState instructions labels
        |> executeSpecialInstr specialInstrList

    let runOnce state =
        let pcAddress = State.registerValue PC state
        let instr = State.getInstruction pcAddress state
        if instr.IsSome then
            execute state instr.Value
            |> State.incrementPC
        else
            State.endExecution state

    let rec runAll state =
        if State.checkEndExecution state then
            state
        else
            state |> runOnce |> runAll
