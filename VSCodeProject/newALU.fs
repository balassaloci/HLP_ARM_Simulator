module NewALU =
    open Machine
    open Instruction
    open CommonOperandFunctions

    type ConditionSuffix1 = | EQ | NE | CS | HS | CC | LO | MI | PL | VS | VC | HI
                           | LS | GE | LT | GT | LE | AL

    type SetBit1 = | UpdateStatus | IgnoreStatus


    type Arithmetic1 = | ADD | SUB | ADC | SBC | RSC
    // type ArithmeticOperation = Arithmetic * SetBit * ConditionSuffix
    type ArithmeticOpCode1 =
        {opcode:Arithmetic1; setBit:SetBit1; condSuffix:ConditionSuffix1}
    // type ArithmeticOperandsPattern = RegOperand * RegOperand * ExecOperand
    type ArithmeticOperands1 = {dest:RegOperand; op1:RegOperand; op2:ExecOperand}
    
    type ALUInstruction1 =
        private {
            operation: ArithmeticOpCode1
            operands: ArithmeticOperands1
        }

    module ALUInstruction1 =
        let private add a b = a + b

        let parse: string -> ALUInstruction1 =
            failwithf "Not implemented"

        let execute (state:State) (instr:ALUInstruction1) =
            let dest = instr.operands.dest
            let op1 = registerOperandValue instr.operands.op1 state
            let op2 = execOperandValue instr.operands.op2 state
            let result = add op1 op2
            State.updateRegister dest result state