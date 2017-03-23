module Execution

open Machine
open InstructionsCommonTypes
open ALU
open Memory
open Branch
open Other
open CommonParserFunctions
open ErrorHandler

type Instruction =
    | ALUInst of ALU.ALUInstruction
    | MemInst of Memory.MemoryInstruction
    | CrInst of Branch.ControlInstruction
    | OInstr of Other.OtherInstruction

module ExecuteParser =
    let private parseLine (instrS: string) :  (string Option * Instruction) =
        let parseInstruction (instrS: string) : Instruction =
            if instrS.Length > 2 then
                match instrS.[..2] with
                | "ADD" | "SUB" | "RSB" | "ADC" | "SBC" | "RSC"
                    | "LSL" | "LSR" | "ASR" | "ROR" | "RRX"
                    | "CMP" | "CMN" | "TST" | "TEQ" | "AND"
                    | "EOR" | "BIC" | "ORR" -> ALUInst <| ALUInstruction.parse instrS
                | "MOV" | "MVN" | "LDR" | "STR" | "LDM"
                    | "STM" | "ADR" -> MemInst <| MemoryInstruction.parse instrS
                | x when not (x = "END" || x.StartsWith("B")) -> 
                       //printfn "calling otherInstruction parse"
                       OInstr <| OtherInstruction.parse instrS

                | _ ->  //printfn "Parsing control instruction %A" instrS
                        CrInst <| ControlInstruction.parse instrS
///                | _ -> failc ("Unrecognized instruction1: " + instrS)
            else
                //printfn "Starting to parse branch instructions"
                match instrS with
                | Prefix "B" _ -> CrInst <| ControlInstruction.parse instrS
                | _ -> failc ("Unrecognized instruction2: " + instrS)

        try
            None, parseInstruction (instrS)
        with
        | CustomException t ->
            try
                match splitInstr instrS with
                | l, i -> Some (l), parseInstruction i
            with
            | _ ->
                failc t

    let parseAll (txt: string) =
        let lines = txt.Split([|'\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.toList

        let rec matchLines lines (memInstr: Instruction array)
                                 (labels:Map<string, int>)
                                 (firstInstr: Instruction array) =
            match lines with
            | x::xn ->
                if (x |> decomment |> trimmer).Length = 0 then
                    matchLines xn memInstr labels firstInstr
                else
                    let parsed = 
                        try
                            parseLine x
                        with
                            CustomException t ->
                                failc ("Error while parsing line: " + x + "\n" + t)
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
        | CrInst bi -> Branch.ControlInstruction.execute state bi
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

    let prepareStateWOParser (instrs:array<Instruction>) labels specialInstrs =
        let specialInstrList = List.ofArray specialInstrs
        State.makeInitialState instrs labels
        |> executeSpecialInstr specialInstrList

    let runOnce state =
        let pcAddress = State.registerValue PC state
        let instr = State.getInstruction pcAddress state
        if instr.IsSome then
            let newState = execute state instr.Value
                        |> State.incrementPC
            let nPC = State.registerValue PC state
            if State.instructionExists nPC newState then newState
            else State.endExecution newState

        else
            State.endExecution state

    let rec runAll state =
        if State.checkEndExecution state then
            state
        else
            state |> runOnce |> runAll
