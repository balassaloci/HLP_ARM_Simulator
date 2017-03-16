module ALU

open Machine
open InstructionsCommonTypes
open Functions
open CommonOperandFunctions


type private 'T ALUOpCode = {opcode:'T; setBit:SetBit; condSuffix:ConditionSuffix}

type private ArithmeticOpCode = Arithmetic ALUOpCode
type private ShiftOpCode = Shift ALUOpCode
type private BitwiseOpCode = Bitwise ALUOpCode 
type private CompareOpCode = {opcode:Compare; condSuffix:ConditionSuffix}

type private ArithmeticOperands = {dest:RegOperand; op1:RegOperand; op2:ExecOperand}
type private ShiftOperands = {dest:RegOperand; op1:RegOperand; op2:MixedOperand}
type private CompareOperands = {op1: RegOperand; op2:ExecOperand}
type private BitwiseOperands = {dest:RegOperand; op1:RegOperand; op2:ExecOperand}


type private ArithmeticInstr = {operation: ArithmeticOpCode; operands: ArithmeticOperands}
type private ShiftInstr = {operation: ShiftOpCode; operands: ShiftOperands}
type private CompareInstr = {operation: CompareOpCode; operands: CompareOperands}
type private BitwiseInstr = {operation: BitwiseOpCode; operands: BitwiseOperands}


type ALUInstruction =
    private
        | AInst of ArithmeticInstr
        | SInst of ShiftInstr
        | CInst of CompareInstr
        | BInst of BitwiseInstr

[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ALUInstruction =
    let parse: string -> ALUInstruction =
        failwithf "Not implemented"
    
    let constructSample () =
        let opcode = {opcode=ADD; setBit=IgnoreStatus; condSuffix=AL}
        let op2' = MixedOp <| Literal 72
        let operands:ArithmeticOperands = {dest=R0; op1=R1; op2=op2'}
        AInst {ArithmeticInstr.operation=opcode; ArithmeticInstr.operands=operands}

    let private executeArithmetic state (instr:ArithmeticInstr)=
        let {opcode=core; setBit=S; condSuffix=cond} = instr.operation
        if conditionHolds state cond then
            let dest = instr.operands.dest
            let op1 = instr.operands.op1
            let op2 = instr.operands.op2
            
            let op1Val = State.registerValue op1 state
            let op2Val = execOperandValue op2 state
            let result = getArithmeticFunction core <| op1Val <| op2Val
            // Need to update CSPR here
            State.updateRegister dest result state
        else
            state

    let private executeShift state (instr:ShiftInstr) =
        let {opcode=core; setBit=S; condSuffix=cond} = instr.operation
        if conditionHolds state cond then
            let dest = instr.operands.dest
            let op1 = instr.operands.op1
            let op2 = instr.operands.op2

            let op1Val = State.registerValue op1 state
            let op2Val = mixedOperandValue op2 state
            let result = getShiftFunction core <| op1Val <| op2Val
            // Need to update CSPR here
            State.updateRegister dest result state
        else
            state

    let private executeCompare state (instr:CompareInstr) =
        let {CompareOpCode.opcode=core; condSuffix=cond} = instr.operation
        if conditionHolds state cond then
            let op1 = instr.operands.op1
            let op2 = instr.operands.op2
            let op1Val = State.registerValue op1 state
            let op2Val = execOperandValue op2 state
            let result = getCompareFunction core <| op1Val <| op2Val
            // Need to update CSPR here
            state
        else
            state

    let private executeBitwise state (instr:BitwiseInstr) =
        let {opcode=core; setBit=S; condSuffix=cond} = instr.operation
        if conditionHolds state cond then
            let ops = instr.operands
            let {BitwiseOperands.dest=dest; op1=op1; op2=op2} = ops
            let op1Val = State.registerValue op1 state
            let op2Val = execOperandValue op2 state
            let result = getBitwiseFunction core <| op1Val <| op2Val
            // Need to update CSPR here
            State.updateRegister dest result state
        else
            state


    let execute (state:State) (instr:ALUInstruction) =
        match instr with
        | AInst ai -> executeArithmetic state ai
        | SInst si -> executeShift state si
        | CInst ci -> executeCompare state ci
        | BInst bi -> executeBitwise state bi