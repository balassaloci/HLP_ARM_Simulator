module Memory

open Machine
open InstructionsCommonTypes
open CommonOperandFunctions
open CommonParserFunctions
open ErrorHandler

//TODO: Special instructions to be added here, don't forget to parse them too ;)
type Move = | MOV | MVN
type SingleMemory = | LDR | STR
type MultipleMemory = | LDM | STM
type MemoryAddress = | ADR

type MemoryMode = | Byte | Word
type StackDirection = | FA | FD | EA | ED | IB | IA | DB | DA
type AddressingMethod = | Offset | PreIndexed | PostIndexed
type AddressExpression = | Label of string | Number of int

type MemoryOpCode<'T,'R> = {opcode: 'T; mode: 'R;
                                condSuffix: ConditionSuffix}

type private MoveOpCode = MemoryOpCode<Move,SetBit>
type private SingleMemoryOpCode = MemoryOpCode<SingleMemory,MemoryMode>
type private MultipleMemoryOpCode = MemoryOpCode<MultipleMemory,StackDirection>
type private LoadAddressOpCode = MemoryOpCode<MemoryAddress,SetBit>

type private MoveOperands = {dest: RegOperand; op1: ExecOperand}
type private SingleMemoryOperands = {op1: RegOperand;
                                     op2: RegOperand;
                                     method: AddressingMethod;
                                     offset: ExecOperand option}
type private MultipleMemoryOperands = {op1: RegOperand;
                                       op2: RegOperand list}
type private LoadAddressOperands = {dest: RegOperand
                                    exp: AddressExpression}

type private MoveInstr = {operation: MoveOpCode; operands: MoveOperands}
type private SingleMemoryInstr = {operation: SingleMemoryOpCode;
                                  operands: SingleMemoryOperands}
type private MultipleMemoryInstr = {operation: MultipleMemoryOpCode
                                    operands: MultipleMemoryOperands}
type private LoadAddressInstr = {operation: LoadAddressOpCode;
                                 operands: LoadAddressOperands}

type MemoryInstruction =
    private
        | MvInst of MoveInstr
        | SInst of SingleMemoryInstr
        | MuInst of MultipleMemoryInstr
        | LInst of LoadAddressInstr 

module MemoryParser =

    type MemoryInstructionTypes =
        | MoveInstructionT of Move
        | SingleMemoryT of SingleMemory
        | MultipleMemoryT of MultipleMemory
        | MemoryAddressT of MemoryAddress

    let getMemoryMode = function
        | "B" -> Byte
        | "" -> Word
        | x -> failc ("Unable to parse Memory Mode: " + x)

    let getUpdateMode = function
        | "EA" -> EA
        | "ED" -> ED
        | "FA" -> FA
        | "FD" -> FD
        | "IA" -> IA
        | "IB" -> IB
        | "DA" -> DA
        | "DB" -> DB
        | x -> failc ("Unable to parse Update Mode: " + x)

    //let getCondB (condBS:string) =
    //    //printfn "getCondB %A" condBS

    //    match condBS.Length with
    //    | 1 -> AL, getMemoryMode condBS
    //    | 2 -> getCond condBS, getMemoryMode ""
    //    | 3 -> //printfn "branch 3: %A" condBS.[..1]
    //           let c = getCond condBS.[..1]
    //           //printfn "gOther c"
    //           let c' = getMemoryMode condBS.[2..2]
    //           //printfn "got c'"
    //           getCond condBS.[..1], getMemoryMode condBS.[2..2]
    //    | _ -> failwith "Invalid condition code"
    
    
    let getBCond = function
        | Prefix "B" cond -> Byte, getCond cond
        | cond -> Word, getCond cond

    let getCondUpmode (condUM:string) =
        match condUM.Length with
        | 2 -> AL, getUpdateMode condUM
        | 4 -> (getCond condUM.[2..3]), (getUpdateMode condUM.[..1])
        | _ -> failc ("Invalid condition or update mode for memory instruction: " + condUM)


    //let remOpenSquareBrkt ( = function
    //    | x::xn when x = '[' -> xn
    //    | _ -> failwith "Invalid register specification"

    let remOpenSquareBrkt (x:string) =
        //printfn "remOpenSquareBrkt %A" x
        if x.StartsWith "[" then x.[1..] else failc ("Invalid register specification: " + x)

    let optRemEnd (x:string) (y:string) = 
        if x.EndsWith y then true, x.[.. x.Length - y.Length - 1] else false, x
    
    let rec getAddressMethod = function
        | (x:string)::[] -> if x.EndsWith "]!" then [x.[..x.Length - 3]], PreIndexed
                            elif x.EndsWith "]" then [x.[..x.Length - 2]], Offset
                            else [x], PostIndexed
        | x::xn -> let v = getAddressMethod xn
                   x :: fst v, snd v
        | x ->  failc "Unable to parse address method"


    let rec getMReg = function
        | (x:string)::xn ->
            if x.Contains "-" then
                let moreRegs = getMReg xn
                let range  = x.Split([|'-'|], 2)
                let startReg = (range.[0] |> trimmer).[1..] |> int
                let endReg = (range.[1] |> trimmer).[1..] |> int
                List.fold
                    (fun acc reg -> (getRegIndex ("R" + string reg)) :: acc)
                    moreRegs [startReg..endReg]
            else
                getRegIndex (x |> trimmer) :: getMReg xn
        | [] -> []

    let unwrapReglist regs =
        let rec fixEnd = function
            | (x:string)::[] -> 
                if x.EndsWith "}" then [x.[..x.Length - 2]]
                else failc "Missing closing bracket '}' from register range"
            | x :: xn -> x :: fixEnd xn
            | _ -> failc ("Unable to parse register ranges")
        
        let fixStart (x:string list) =
            let x1 = x.Head.Trim()
            if x1.StartsWith "{" then
                x1.[1..] :: x.Tail
            else
                failc "Missing opening bracket '{' from register range"
        
        regs |> fixStart |> fixEnd |> getMReg

    let getMemoryInstruction instruction =
        match instruction with
        | "MOV" -> MoveInstructionT MOV
        | "MVN" -> MoveInstructionT MVN
        | "LDR" -> SingleMemoryT LDR
        | "STR" -> SingleMemoryT STR
        | "LDM" -> MultipleMemoryT LDM
        | "STM" -> MultipleMemoryT STM
        | "ADR" -> MemoryAddressT ADR
        | _ -> failc "Unable to parse memory instruction %A" instruction
    
    let parseMoveInstruction (i:Move) (instrStr:string) splitOper =
        //printfn "Parsing move instruction"

        match splitOper with
        | op1S :: op2S :: restS ->

            let scode = getSCond instrStr.[3..] 
            let op1 = op1S |> getRegIndex
            //printfn "parsing move instr %A" (op2S, restS)
            let op2 : ExecOperand = parseExecOperand op2S restS
            //printfn "parsing move instr %A" op2
            //printfn "%A" op2
            let opcode : MoveOpCode = {opcode = i; mode = fst scode; condSuffix = snd scode}
            let operands : MoveOperands = {dest = op1; op1 = op2}
            let instr : MemoryInstruction = MvInst {operation = opcode; operands = operands}
            instr

        | _ -> failc "Unable to parse move instruction: "
    
    let parseSingleMemoryInstruction (i:SingleMemory) (instrStr:string) (splitOper:string list) =
        //printfn "Parsing single memory instruction"
        let conds = getBCond instrStr.[3..]
                    //if instrStr.Length > 3 then
                    //    getCondB instrStr.[3..]

                    //else AL, getMemoryMode ""

        //printfn "Cond code done"
        let op1 : RegOperand = splitOper.Head |> getRegIndex
        //printfn "Op1 done"
        let split' = splitOper.Tail
        //printfn "split' done"
        let op2'  = split'.Head |> trimmer |> remOpenSquareBrkt
        //printfn "remOpenS done"
        let op2 = snd (optRemEnd op2' "]") |> getRegIndex
        //printfn "Op2 done %A" split'.Tail

        let cleanAddrMethod = getAddressMethod (split'.Tail)
        //printfn "CleanAddrMethod done"
        let aMethod = snd cleanAddrMethod
        let cleanOps:string list = fst cleanAddrMethod
        let offset = parseExecOperand cleanOps.Head cleanOps.Tail
        //printfn "parseExecOperand done"

        //let op2Method = optRemEnd op2' "]"
        //let aMethod:AddressingMethod =
        //    if fst op2Method then PreIndexed else PostIndexed
        
        //let op2 : RegOperand = snd op2Method |> getRegIndex
        //let split'' = split'.Tail
        //let offset = parseExecOperand (split''.Head) (split''.Tail)


        //type private SingleMemoryOperands = {op1: RegOperand;
        //                                     op2: RegOperand;
        //                                     method: AddressingMethod;
        //                                     offset: ExecOperand}

        //{opcode: 'T; mode: 'R; condSuffix: ConditionSuffix}
        //SingleMemoryInstr{ operation: SingleMemoryOpCode; operands: SingleMemoryOperands}

        let operands : SingleMemoryOperands =
            {op1 = op1; op2 = op2; method = aMethod; offset = offset}
        
        let opCode : SingleMemoryOpCode =
            {opcode = i; mode = fst conds; condSuffix = snd conds}
       
        let instruction : SingleMemoryInstr =
            {operation = opCode; operands = operands}
        
        //printfn "%A"
            //(instruction.operation.opcode, instruction.operation.mode, 
            //instruction.operation.condSuffix,
            //instruction. operands.op1, 
            //instruction. operands.op2, 
            //instruction. operands.method, 
            //instruction. operands.offset)
             
        instruction |> SInst

    let parseMultipleMemoryInstruction (i:MultipleMemory) (instrStr:string)
                                                    (splitOper:string list) =
        let condUpMode = getCondUpmode instrStr.[3..]
        //printfn "condUpMode %A" splitOper.Head

        let op1 = splitOper.Head |> getRegIndex
        //printfn "op1 %A" splitOper.Tail

        let op2 = unwrapReglist splitOper.Tail
        //printfn "op2"

        let opcode : MultipleMemoryOpCode = {opcode = i;
                                             mode = snd condUpMode;
                                             condSuffix = fst condUpMode}

        let operands : MultipleMemoryOperands = {op1 = op1; op2 = op2}
        let instr : MultipleMemoryInstr = {operation = opcode;
                                            operands = operands}

        instr |> MuInst

    let private parseADR (i:MemoryAddress) (instrStr:string) splitOper =
        printfn "%A" splitOper
        match splitOper with
        | op1S :: op2S :: [] ->
            let scode = getSCond instrStr.[3..]
            printfn "scode done"

            let op1 = op1S |> getRegIndex

            printfn "reg1 done"
            let op2 : AddressExpression =
                try
                   // Number <| parseLiteral <| op2S
                   let a = parseLiteral <| op2S.Trim()
                   Number <| a
                with
                | _ -> Label <| op2S.Trim()

            //let isNumeric a = fst (System.Int32.TryParse(a))
            //let parseLiteral a =
            printfn "reg2 done"
            //
            //let op2 : ExecOperand = parseExecOperand op2S restS
            let opcode : LoadAddressOpCode = {opcode = i; mode = fst scode; condSuffix = snd scode}
            let operands : LoadAddressOperands = {dest = op1; exp = op2}
            let instr : LoadAddressInstr =  {operation = opcode; operands = operands}
            instr |> LInst
        | _ ->  printfn "adr instruction failed"
                failc "Unable to parse move instruction: "

    let parseLine line =
        let cleanLine = line |> decomment |> trimmer |> splitInstr
        let instrStr : string = fst cleanLine
        let paramStr : string = snd cleanLine
        let splitOper = splitOperands paramStr
        let instr = getMemoryInstruction instrStr.[0..2]

        match instr with
        | MoveInstructionT i -> parseMoveInstruction i instrStr splitOper
        | SingleMemoryT i -> parseSingleMemoryInstruction i instrStr splitOper
        | MultipleMemoryT i -> parseMultipleMemoryInstruction i instrStr splitOper
        | MemoryAddressT i -> parseADR i instrStr splitOper
        //| _ -> failwith "Not implemented yet"



[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MemoryInstruction =
    open MemoryParser
    let parse: string -> MemoryInstruction =
        parseLine
        
        //failc "Not implemented"

    let private applyMoveFunction (core:Move) (v:int64) =
        match core with
        | MOV -> int v
        | MVN -> int ~~~v

    let private loadSingleRegister (b: MemoryMode) (dest: RegOperand) (address: int) state =
        match b with
        | Byte ->
                let value = State.getByteFromMemory address state
                State.updateRegister dest value state
        | Word ->
                if (address % 4 = 0) then
                    let value = State.getWordFromMemory address state
                    State.updateRegister dest value state
                else
                    failc "address should be a multiple of 4"

    let private storeSingleRegister (b: MemoryMode) (src: RegOperand) (address: int) state =
        match b with
        | Byte ->
                let firstByteValue = (State.registerValue src state) &&& 255
                State.updateByteInMemory address firstByteValue state
        | Word ->
                if (address % 4 = 0) then
                    let value = State.registerValue src state
                    State.updateWordInMemory address value state
                else
                    failc "address should be a multiple of 4"
    
    let private getSingleRegisterMemoryFunction = function
        | LDR -> loadSingleRegister
        | STR -> storeSingleRegister

    let private stackOperationParams (dir:MultipleMemory*StackDirection) (rList:RegOperand list) =
        //return regList * offset for each iteration * address offset * register offset
        match dir with
        | _,DA | STM,ED | LDM,FA -> (List.rev rList, -4, 0, 0)
        | _,DB | STM,FD | LDM,EA -> (List.rev rList, -4, -4, 4)
        | _,IA | STM,EA | LDM,FD -> (rList, 4, 0, 0)
        | _,IB | STM,FA | LDM,ED -> (rList, 4, 4, -4)

    let private store (param: RegOperand list * int * int * int) adrReg state =
        let address = State.registerValue adrReg state
        if address % 4 = 0 then
            let rList, iOffset, aOffset, rOffset = param
            let rec storeRegisters adr (regList: RegOperand list) s =
                match regList with
                | r::t ->
                    let rValue = State.registerValue r s
                    let newState = State.updateWordInMemory adr rValue s
                    storeRegisters (adr+iOffset) t newState
                | _ -> State.updateRegister adrReg (adr+rOffset) state
            storeRegisters (address+aOffset) rList state
        else
            failc "address should be a multiple of 4"

    let private load (param: RegOperand list * int * int * int) adrReg  state =
        let address = State.registerValue adrReg state
        if address % 4 = 0 then
            let rList, iOffset, aOffset, rOffset = param
            let rec loadRegisters adr (regList:RegOperand list) s =
                match regList with
                | r::t ->
                    let mValue = State.getWordFromMemory adr s
                    let newState = state |> State.deleteWordInMemory adr
                                        |> State.updateRegister r mValue
                    loadRegisters (adr+iOffset) t newState
                | _ -> State.updateRegister adrReg (adr+rOffset) state
            loadRegisters (address+aOffset) rList state
        else
            failc "address should be a multiple of 4"

    let private addressExpressionValue (expression: AddressExpression) state =
        match expression with
        | Label l -> State.getLabelAddress l state
        | Number n -> n


//-----------------------------------------------------------------------
    let private executeMove state (instr:MoveInstr) =
        let {opcode = core; mode = setBit; condSuffix = cond} = instr.operation
        if conditionHolds state cond then
            let dest,op1 = instr.operands.dest, instr.operands.op1
            let v,carry = execOperandValue op1 state
            let value = applyMoveFunction core v 
            match setBit with
            | UpdateStatus -> 
                let n = (value < 0)
                let z = (value = 0)
                let c = (carry = 1)
                state |> State.updateStatusBit N n
                      |> State.updateStatusBit Z z
                      |> State.updateStatusBit C c
                      |> State.updateRegister dest value
            | IgnoreStatus -> State.updateRegister dest value state

        else
            state

    let private executeSingleRegisterMemoryInstruction state (instr:SingleMemoryInstr) =
        let {opcode = core; mode = memoryMode; condSuffix = cond} = instr.operation
        if conditionHolds state cond then
            let op1,op2 = instr.operands.op1, instr.operands.op2
            let method,offset = instr.operands.method, instr.operands.offset
            match method with 
            | Offset ->
                if Option.isSome offset then
                    let v,_ = execOperandValue (Option.get offset) state
                    let address = 
                        (State.registerValue op2 state) + int v
                    getSingleRegisterMemoryFunction core <|memoryMode <|op1 <|address <|state
                else
                    let v = 0
                    let address = 
                        (State.registerValue op2 state) + int v
                    getSingleRegisterMemoryFunction core <|memoryMode <|op1 <|address <|state
            | PreIndexed ->
                if Option.isSome offset then
                    let v,_ = execOperandValue (Option.get offset) state
                    let address =
                        (State.registerValue op2 state) + int v
                    let newState = State.updateRegister op2 address state
                    getSingleRegisterMemoryFunction core <|memoryMode <|op1 <|address <|newState
                else
                    let v = 0
                    let address =
                        (State.registerValue op2 state) + int v
                    let newState = State.updateRegister op2 address state
                    getSingleRegisterMemoryFunction core <|memoryMode <|op1 <|address <|newState
            | PostIndexed ->
                if Option.isSome offset then
                    let address = State.registerValue op2 state
                    let v,_ = execOperandValue (Option.get offset) state
                    let newState = State.updateRegister op2 (address + int v) state
                    getSingleRegisterMemoryFunction core <|memoryMode <|op1 <|address <|newState
                else
                    let address = State.registerValue op2 state
                    let v = 0
                    let newState = State.updateRegister op2 (address + int v) state
                    getSingleRegisterMemoryFunction core <|memoryMode <|op1 <|address <|newState
        else
            state

    let private executeMultipleRegisterMemoryInstruction state (instr:MultipleMemoryInstr) = 
        let {opcode = core; mode = sDirection; condSuffix = cond} = instr.operation
        if conditionHolds state cond then
            let op1,op2 = instr.operands.op1, instr.operands.op2
            let parameters = stackOperationParams (core,sDirection) op2
            match core with
            | LDM -> load parameters op1 state
            | STM -> store parameters op1 state
        else
            state

    let private executeLoadAddressInstruction state (instr:LoadAddressInstr) = 
        let cond = instr.operation.condSuffix
        if conditionHolds state cond then
            let dest, exp = instr.operands.dest,instr.operands.exp
            let value = addressExpressionValue exp state
            State.updateRegister dest value state
        else
            state

    let execute state (instr:MemoryInstruction) =
        match instr with
        |MvInst mvi -> executeMove state mvi
        |SInst si -> executeSingleRegisterMemoryInstruction state si
        |MuInst mui -> executeMultipleRegisterMemoryInstruction state mui
        |LInst li -> executeLoadAddressInstruction state li
