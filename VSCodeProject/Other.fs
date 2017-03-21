module Other
open Machine
open CommonParserFunctions

type DeclareWord = | DCD | DCB
type DeclareConstant = | EQU
type FillMemory = | FILL

type private DeclareWordInstr = {opcode:DeclareWord; label: string; op1:int list}
type private DeclareConstantInstr = {opcode:DeclareConstant; label: string; op1:int} //op1 can be an expression
type private FillMemoryInstr = {opcode:FillMemory; label: option<string>; op1:int}

type OtherInstruction =
    private 
        | DwInst of DeclareWordInstr
        | DcInst of DeclareConstantInstr
        | FInst of FillMemoryInstr

module OtherParser =

    //let getMemoryInstruction instruction =
    //    match instruction with
    //    | "MOV" -> MoveInstructionT MOV
    //    | "MVN" -> MoveInstructionT MVN
    //    | "LDR" -> SingleMemoryT LDR
    //    | "STR" -> SingleMemoryT STR
    //    | "LDM" -> MultipleMemoryT LDM
    //    | "STM" -> MultipleMemoryT STM
    //    | "ADR" -> MemoryAddressT ADR
    //    | _ -> failwithf "Unable to parse memory instruction %A" instruction



    let parseLine (line:string) =
        printfn "Parsing other instruction %A" line
        let cleanLine = line |> decomment |> trimmer |> splitInstr
        printfn "cleanline done"
        let label = fst cleanLine
        printfn "label done %A" label
        let instruction = snd cleanLine |> splitInstr
        printfn "instruction split done %A" instruction
        let opcode' : string = fst instruction
        printfn "opcode split but not parsed %A" opcode'
        let prms = snd instruction |> splitOperands
        printf "params %A" prms


        printfn "Parsing stuff based on opcode %A" opcode'

        failwithf "sorry, unable to parse based on opcode"

        match opcode' with
        | "DCD" | "DCB" ->
            failwith "DCD?DCB branch open"

            let op1s = List.map parseLiteral prms
            printfn "op1s parsed %A" op1s

            let opcode =
                match opcode' with
                | "DCD" -> DCD
                | "DCB" -> DCB
            
            printfn "opcode parsed %A" opcode
            let instr : DeclareWordInstr = {opcode = opcode;
                                            label=label;
                                            op1=op1s}

            printfn "Parsing done for instruction %A" instr                     
            DwInst <| instr
        | "EQU" ->
            failwith "Parse EQU instr %A" opcode'
            let op1:int = parseLiteral prms.[0]
            let instr : DeclareConstantInstr = {opcode = EQU;
                                                label = label;
                                                op1 = op1}
            DcInst <| instr
        | _ ->  //printfn "Parse other instruction type failed"
                failwith "Other instruction not defined"


        //printfn "%A" (label, opcode, prms)

    

[<RequireQualifiedAccess;
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OtherInstruction =
    let parse instrS =
        OtherParser.parseLine instrS

        //failwith "Not implemented"
        
    let rec private declareWords state address wList =
        match wList with
        | h::t -> 
            let newState = State.updateWordInMemory address h state
            declareWords newState (address + 4) t
        | _ -> State.updateMemoryAddress address state

    let rec private declareBytes state address bList =
        match bList with
        | h::t ->
            if h < 256 || h >= -128 then
                let newState = State.updateByteInMemory address h state
                declareBytes newState (address + 1) t
            else
                failwithf("Value should be a byte")
        | _ ->
            if address % 4 = 0 then
                State.updateMemoryAddress address state
            else
                let newAddress = address + 4 - (address % 4)
                State.updateMemoryAddress newAddress state
    
    let rec private fillMemory state address n =
        match n with
        | n when n > 0 -> 
            let newState = State.updateWordInMemory address 0 state
            fillMemory newState (address+4) (n-4)
        | 0 -> State.updateMemoryAddress address state
        | _ -> failwithf("FILL instruciton: n cannot be negative") 
        

    let private executeDeclareWordInstruction state (instr:DeclareWordInstr) =
        let core,label,nList = instr.opcode,instr.label,instr.op1
        let address = State.getMemoryAddress state
        match core with
        | DCD -> declareWords state address nList
        | DCB -> declareBytes state address nList

    let private executeFillMemoryInstruction state (instr:FillMemoryInstr) =
        let label, n = instr.label,instr.op1
        if n % 4 = 0 then
            let address = State.getMemoryAddress state
            match label with
            | Some l -> 
                let s = State.addLabelAddress l address state
                fillMemory s address n
            | None -> fillMemory state address n
        else
            failwithf("Fill instruction: n must be a multiple of 4")

    let private executeDeclareConstantInstruction state 
                                                  (instr:DeclareConstantInstr) =
        let label,n = instr.label,instr.op1
        State.addLabelAddress label n state
    
    let execute state (instr:OtherInstruction) =
        match instr with
        |DwInst dwI -> executeDeclareWordInstruction state dwI
        |DcInst dcI -> executeDeclareConstantInstruction state dcI
        |FInst fI -> executeFillMemoryInstruction state fI
