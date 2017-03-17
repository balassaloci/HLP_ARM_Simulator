module Memory

open Machine
open InstructionsCommonTypes
open CommonOperandFunctions

type Move = | MOV | MVN
type SingleMemory = | LDR | STR
type MemoryMode = | Byte | Word
type MultipleMemory = | LDM | STM
type StackDirection = | FA | FD | EA | ED | IB | IA | DB | DA
type MemoryAddress = | ADR
type AddressingMethod = | Offset | PreIndexed | PostIndexed
type AddressExpression = | Label of string | Number of int

type MemoryOpCode<'T,'R> = {opcode: 'T; mode: 'R;
                                condSuffix: ConditionSuffix}

type private MoveOpCode = MemoryOpCode<Move,SetBit>
type private SingleMemomryOpCode = MemoryOpCode<SingleMemory,MemoryMode>
type private MultipleMemoryOpCode = MemoryOpCode<MultipleMemory,StackDirection>
type private LoadAddressOpCode = MemoryOpCode<MemoryAddress,SetBit>

type private MoveOperands = {dest: RegOperand; op1: ExecOperand}
type private SingleMemoryOperands = {op1: RegOperand;
                                     op2: RegOperand;
                                     method: AddressingMethod;
                                     offset: ExecOperand}
type private MultipleMemoryOperands = {op1: RegOperand;
                                       op2: RegOperand list}
type private LoadAddressOperands = {dest: RegOperand
                                    exp: AddressExpression}

type private MoveInstr = {operation: MoveOpCode; operands: MoveOperands}
type private SingleMemoryInstr = {operation: SingleMemomryOpCode;
                                  operands: SingleMemoryOperands}
type private MultipleMemoryInstr = {operation: MultipleMemoryOpCode
                                    operands: MultipleMemoryOperands}
type private LoadAddressInstr = {operation: LoadAddressOpCode;
                                 operands: LoadAddressOperands}

type MemoryInstruction =
    private {
        opcode: string
        operandsss: string list
    }

[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MemoryInstruction =

    let private applyMoveFunction (core:Move) (v:int64) =
        match core with
        | MOV -> int v
        | MVN -> int ~~~v


    let loadSingleRegister (b: MemoryMode) (dest: RegOperand) (address: int) (state: State) =
        match b with
        | Byte ->
                let value = State.getByteFromMemory address state
                State.updateRegister dest value state
        | Word ->
                if (address % 4 = 0) then
                    let value = State.getWordFromMemory address state
                    State.updateRegister dest value state
                else
                    failwithf("address should be a multiple of 4")

    let storeSingleRegister (b: MemoryMode) (src: RegOperand) (address: int) (state: State) =
        match b with
        | Byte ->
                let firstByteValue = (State.registerValue src state) &&& 255
                State.updateByteInMemory address firstByteValue state
        | Word ->
                if (address % 4 = 0) then
                    let value = State.registerValue src state
                    State.updateWordInMemory address value state
                else
                    failwithf("address should be a multiple of 4")

    let loadFA (adrReg: RegOperand) (destRegList: RegOperand list) (state: State) = None

    let loadFD (adrReg: RegOperand) (destRegList: RegOperand list) (state: State) = None

    let loadEA (adrReg: RegOperand) (destRegList: RegOperand list) (state: State) =None

    let loadED (adrReg: RegOperand) (destRegList: RegOperand list) (state: State) = None

    let storeFA (adrReg: RegOperand) (destRegList: RegOperand list) (state: State) =
        let address = State.registerValue adrReg state
        let rec storeRegisters (adr:int) (regList: RegOperand list) (s: State) =
            match regList with
            | h::t -> 
                let value = State.registerValue h s
                let newState = State.updateWordInMemory adr value s
                storeRegisters (adr+4) t newState
            | _ -> State.updateRegister adrReg adr s
        storeRegisters (address+4) destRegList state

    let storeFD (adrReg: RegOperand) (destRegList: RegOperand list) (state: State) = None

    let storeEA (adrReg: RegOperand) (destRegList: RegOperand list) (state: State) =
        let address = State.registerValue adrReg state
        let rec storeRegisters (adr:int) (regList: RegOperand list) (s: State) =
            match regList with
            | h::t -> 
                let value = State.registerValue h s
                let newState = State.updateWordInMemory adr value s
                storeRegisters (adr+4) t newState
            | _ -> State.updateRegister adrReg adr s
        storeRegisters address destRegList state

    let storeED (adrReg: RegOperand) (destRegList: RegOperand list) (state: State) = None

    
    let getSingleRegisterMemoryFunction = function
        | LDR -> loadSingleRegister
        | STR -> storeSingleRegister

    let getMultipleRegisterMemoryFunctions = function
        | LDM,FA | LDM,DA -> ()//loadFA
        | LDM,FD | LDM,IA -> ()//loadFD
        | LDM,EA | LDM,DB -> ()//loadEA
        | LDM,ED | LDM,IB -> ()//loadED
        | STM,FA | STM,IB -> ()//storeFA
        | STM,FD | STM,DB -> ()//storeFD
        | STM,EA | STM,IA -> ()//storeEA
        | STM,ED | STM,DA -> ()//storeED


    let addressExpressionValue (expression: AddressExpression) (state: State) =
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
                let n = if value < 0 then true else false
                let z = if value = 0 then true else false
                let c = if carry = 1 then true else false
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
                let v,_ = execOperandValue offset state
                let address = 
                    (State.registerValue op2 state) + int v
                getSingleRegisterMemoryFunction core <|memoryMode <|op1 <|address <|state
            | PreIndexed ->
                let v,_ = execOperandValue offset state
                let address =
                    (State.registerValue op2 state) + int v
                let newState = State.updateRegister op2 address state
                getSingleRegisterMemoryFunction core <|memoryMode <|op1 <|address <|newState
            | PostIndexed ->
                let address = State.registerValue op2 state
                let v,_ = execOperandValue offset state
                let newState = State.updateRegister op2 (address + int v) state
                getSingleRegisterMemoryFunction core <|memoryMode <|op1 <|address <|newState
        else
            state

    let private executeMultipleRegisterMemoryInstruction () = None
    let private executeLoadAddressInstruction state (instr:LoadAddressInstr) = 
        let cond = instr.operation.condSuffix
        if conditionHolds state cond then
            let dest, exp = instr.operands.dest,instr.operands.exp
            let value = addressExpressionValue exp state
            State.updateRegister dest value state
        else
            state

    let execute (state:State) (instr:MemoryInstruction) =
        failwithf "Not implemented"