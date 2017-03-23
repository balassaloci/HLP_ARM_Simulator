module Other
open Machine
open CommonParserFunctions
open ErrorHandler

type DeclareWord = | DCD | DCB
type DeclareConstant = | EQU
type FillMemory = | FILL

type private DeclareWordInstr = {opcode:DeclareWord; label: string; op1:int list}
type private DeclareConstantInstr = {opcode:DeclareConstant; label: string; op1:int} 
type private FillMemoryInstr = {opcode:FillMemory; label: string option; op1:int}

type OtherInstruction =
    private 
        | DwInst of DeclareWordInstr
        | DcInst of DeclareConstantInstr
        | FInst of FillMemoryInstr

module OtherParser =


    let parseLine (line:string) =
        let cleanLine = line |> decomment |> trimmer |> splitInstr
        let label = fst cleanLine
        if label = "FILL" then
            let num = snd cleanLine |> int
            let instr: FillMemoryInstr =
                {opcode=FILL; label = None; op1 = num}
            FInst <| instr
        else
            let instruction = snd cleanLine |> splitInstr
            let opcode' : string = fst instruction

            match opcode' with
            | "DCD" | "DCB" ->
                let prms: string list = snd instruction |> splitOperands
                let op1s = List.map (fun (x:string) -> x.Trim() |> parseLiteral) prms
                let opcode = if opcode' = "DCD" then DCD else DCB
                    
                let instr : DeclareWordInstr = {opcode = opcode;
                                                label=label;
                                                op1=op1s}
                DwInst <| instr
            | "EQU" ->
                let prms = snd instruction |> splitOperands
                let op1:int = parseLiteral prms.[0]
                let instr : DeclareConstantInstr = {opcode = EQU;
                                                    label = label;
                                                    op1 = op1}
                DcInst <| instr

            | "FILL" ->
                let num = snd instruction |> int
                let instr: FillMemoryInstr =
                    {opcode=FILL; label = Some label; op1 = num}

                FInst <| instr
            | _ -> 
                failc ("Unrecognized instruction: " + label)

            
    

[<RequireQualifiedAccess;
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OtherInstruction =
    let parse instrS =
        //printfn "otherInstruction.parse %A" instrS

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
                failc "Value should be a byte"
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
        | _ -> failc "FILL instruciton: n cannot be negative"
        

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
            failc "Fill instruction: n must be a multiple of 4"

    let private executeDeclareConstantInstruction state 
                                                  (instr:DeclareConstantInstr) =
        let label,n = instr.label,instr.op1
        State.addLabelAddress label n state
    
    let execute state (instr:OtherInstruction) =
        match instr with
        |DwInst dwI -> executeDeclareWordInstruction state dwI
        |DcInst dcI -> executeDeclareConstantInstruction state dcI
        |FInst fI -> executeFillMemoryInstruction state fI

    let makeDCDSample (i:DeclareWord) (l:string) (nL: int list) =
        DwInst {DeclareWordInstr.opcode = i;
               DeclareWordInstr.label = l;
               DeclareWordInstr.op1 = nL}
