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
    private
        |MvInst of MoveInstr
        |SInst of SingleMemoryInstr
        |MuInst of MultipleMemoryInstr
        |LInst of LoadAddressInstr 

[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MemoryInstruction =

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
                    failwithf("address should be a multiple of 4")

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
                    failwithf("address should be a multiple of 4")
    
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
            failwithf("address should be a multiple of 4")

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
            failwithf("address should be a multiple of 4")

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