module ALU

open Machine
open InstructionsCommonTypes
open Functions
open CommonOperandFunctions
open CommonParserFunctions
open ErrorHandler


type private 'T ALUOpCode = {opcode:'T; setBit:SetBit;
                             condSuffix:ConditionSuffix}

type private ArithmeticOpCode = Arithmetic ALUOpCode
type private ShiftOpCode = Shift ALUOpCode
type private BitwiseOpCode = Bitwise ALUOpCode
type private CompareOpCode = {opcode:Compare; condSuffix:ConditionSuffix}

type private ArithmeticOperands = {dest:RegOperand;
                                   op1:RegOperand; op2:ExecOperand}
type private ShiftOperands = {dest:RegOperand; op1:RegOperand; op2:MixedOperand}
type private CompareOperands = {op1: RegOperand; op2:ExecOperand}
type private BitwiseOperands = {dest:RegOperand;
                                op1:RegOperand; op2:ExecOperand}


type private ArithmeticInstr = {operation: ArithmeticOpCode;
                                operands: ArithmeticOperands}
type private ShiftInstr = {operation: ShiftOpCode; operands: ShiftOperands}
type private CompareInstr = {operation: CompareOpCode;
                             operands: CompareOperands}
type private BitwiseInstr = {operation: BitwiseOpCode;
                             operands: BitwiseOperands}

type ALUInstruction =
    private
        | AInst of ArithmeticInstr
        | SInst of ShiftInstr
        | CInst of CompareInstr
        | BInst of BitwiseInstr

module ALUParser =
    let private parseArithmeticOperands (ops: string list) =
       match ops with
       | destS :: op1S :: op2S :: restS ->
           let dest = destS |> getRegIndex
           let op1 = op1S |> getRegIndex
           let op2 : ExecOperand = parseExecOperand op2S restS
           let compiled : ArithmeticOperands =
               {dest= dest; op1=op1; op2=op2}

           compiled
       | _ -> failc "Unrecognized operand pattern"

    let private parseBitwiseOperands (ops: string list) =
       match ops with
       | destS :: op1S :: op2S :: restS ->
           let dest = destS |> getRegIndex
           let op1 = op1S |> getRegIndex
           let op2 : ExecOperand = parseExecOperand op2S restS
           let compiled : BitwiseOperands =
               {dest= dest; op1=op1; op2=op2}

           compiled
       | _ -> failc "Unrecognized operand pattern"

    let private parseShiftOperands (ops: string list) =
        match ops with
        | destS :: op1S :: op2S :: [] ->
            let dest = destS |> getRegIndex
            let op1 = op1S |> getRegIndex
            let op2 = op2S |> parseMixedOp
            let compiled : ShiftOperands = {dest = dest; op1=op1; op2=op2}
            compiled
        | _ -> failc "Unrecognized operand pattern"

    let private parseCompareOperands (ops: string list) =
        match ops with
        | op1S :: op2S :: restS ->
            let op1 = op1S |> getRegIndex
            let op2 : ExecOperand = parseExecOperand op2S restS
            let compiled : CompareOperands =
                {op1 =  op1; op2 = op2}
            compiled
        | _ -> failc "Unrecognized operand pattern"


    let parseLine line =

        let cleanLine = line |> decomment |> trimmer |> splitInstr

        let instrStr : string = fst cleanLine
        let paramStr : string = snd cleanLine
        let splitOper = splitOperands paramStr
        let instr = getALUInstruction instrStr.[0..2]
        let scode = getSCond instrStr.[3..]

        match instr with
        | ArithmeticInstructionT i ->
            let AOpCode = {opcode = i; setBit = (fst scode);
                            condSuffix = (snd scode)}
            let opers = parseArithmeticOperands splitOper
            AInst {ArithmeticInstr.operation = AOpCode; operands = opers}
                       
        | ShiftInstructionT i ->
            let AOpCode : ShiftOpCode = {opcode = i; setBit = (fst scode);
                            condSuffix = (snd scode)}
            let opers : ShiftOperands = parseShiftOperands splitOper
            SInst {operation = AOpCode; operands = opers}

        | CompareInstructionT i ->
            let AOpCode : CompareOpCode = {opcode = i; condSuffix = (snd scode)}
            let opers : CompareOperands = parseCompareOperands splitOper
            CInst {operation = AOpCode; operands = opers}

        | BitwiseInstructionT i ->
            let AOpCode : BitwiseOpCode = {opcode = i; setBit = (fst scode); condSuffix = (snd scode)}
            let opers = parseBitwiseOperands splitOper
            BInst {operation = AOpCode; operands = opers}



[<RequireQualifiedAccess;
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ALUInstruction =
    open ALUParser
    let parse: string -> ALUInstruction =
        parseLine

    let constructSample () =
        let opcode = {opcode=ADD; setBit=IgnoreStatus; condSuffix=AL}
        let op2' = MixedOp <| Literal 72
        let operands:ArithmeticOperands = {dest=R0; op1=R1; op2=op2'}
        AInst {ArithmeticInstr.operation=opcode;
               ArithmeticInstr.operands=operands}

    /// Check whether R13 and PC are correctly used
    let private areValidArithmeticOperands opcode dest op1 op2 =
            true

    let private executeArithmetic state (instr:ArithmeticInstr)=
        let {opcode=core; setBit=S; condSuffix=cond} = instr.operation
        if conditionHolds state cond then
            let dest = instr.operands.dest
            let op1 = instr.operands.op1
            let op2 = instr.operands.op2
            if not <| areValidArithmeticOperands core dest op1 op2 then
                failc "Wrong operands supplied"

            let op1Val = int64 <| State.registerValue op1 state
            // Ignore carry here
            let op2Val, _ = execOperandValue op2 state
            let carry = int64 <| if conditionHolds state CS then 1 else 0
            let result = applyArithmeticFunction core carry op1Val op2Val

            if S = UpdateStatus then
                let addition = match core with
                               | ADD | ADC -> true
                               | SUB | RSB | SBC | RSC -> false
                updateArithmeticCSPR state result addition
            else
                state
            |> State.updateRegister dest (int result)
        else
            state

    let private executeShift state (instr:ShiftInstr) =
        let {opcode=core; setBit=S; condSuffix=cond} = instr.operation
        if conditionHolds state cond then
            let dest = instr.operands.dest
            let op1 = instr.operands.op1
            let op2 = instr.operands.op2

            let op1Val = State.registerValue op1 state
            let op2Val =
                match op2 with
                | Register r -> State.registerValue r state |> (&&&) 0xFF
                | Literal x ->
                    if x < 256 then
                        x
                    else
                        raise <| CustomException "Constant second operand in shifts must be less than 256"

            let carry = if conditionHolds state CS then 1 else 0
            let {body=result; carry=carry} =
                applyShiftFunction core carry op1Val op2Val

            if S = UpdateStatus then
                if op2Val <> 0 then State.updateStatusBit C (carry=1) state else state
                |> checkZero (int64 result)
                |> checkNegative (int64 result)
            else
                state
            |> State.updateRegister dest (int result)
        else
            state

    let private executeCompare state (instr:CompareInstr) =
        let {CompareOpCode.opcode=core; condSuffix=cond} = instr.operation
        if conditionHolds state cond then
            let op1 = instr.operands.op1
            let op2 = instr.operands.op2
            let op1Val = int64 <| State.registerValue op1 state
            let op2Val, carry = execOperandValue op2 state
            let result = getCompareFunction core <| op1Val <| op2Val

            match core with
            | CMN | CMP -> updateArithmeticCSPR state result (core = CMN)
            | TST | TEQ ->

                if carry = 1 then State.updateStatusBit C (carry=1) state else state
                |> checkNegative result
                |> checkZero result
        else
            state

    let private executeBitwise state (instr:BitwiseInstr) =
        let {opcode=core; setBit=S; condSuffix=cond} = instr.operation
        if conditionHolds state cond then
            let ops = instr.operands
            let {BitwiseOperands.dest=dest; op1=op1; op2=op2} = ops
            let op1Val = int64 <| State.registerValue op1 state
            let op2Val, carry = execOperandValue op2 state
            let result = getBitwiseFunction core <| op1Val <| op2Val
            if S = UpdateStatus then
                if carry = 1 then State.updateStatusBit C (carry=1) state else state
                |> checkNegative result
                |> checkZero result
                |> State.updateRegister dest (int result)
            else
                state
        else
            state


    let execute state (instr:ALUInstruction) =
        match instr with
        | AInst ai -> executeArithmetic state ai
        | SInst si -> executeShift state si
        | CInst ci -> executeCompare state ci
        | BInst bi -> executeBitwise state bi
