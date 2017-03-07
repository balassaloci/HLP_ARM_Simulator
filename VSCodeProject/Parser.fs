module Parser
open Machine
open Instruction

let ParseLine (line : string) : ALUInstruction Option= 
    let splitter (s:string) = s.Split([|','; ' '; '\n'; '\n'; '\r'; '\f'|], 
                                System.StringSplitOptions.RemoveEmptyEntries)
    
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

    let lsplit : string list = splitter line |> Array.toList
    let isNumeric a = fst (System.Int32.TryParse(a))
    let repack (a, (b, c)) = (a, b, c)

    let getRegIndex r =
        match r with
        | "R0" -> R0   
        | "R1" -> R1  
        | "R2" -> R2  
        | "R3" -> R3  
        | "R4" -> R4  
        | "R5" -> R5  
        | "R6" -> R6  
        | "R7" -> R7  
        | "R8" -> R8  
        | "R9" -> R9  
        | "R10" -> R10 
        | "R11" -> R11 
        | "R12" -> R12 
        | "R13" -> R13 
        | "LR" -> LR  
        | "PC" -> PC 
        // This is another register but not addressable by user 
        | "CSPR" | _ -> failwithf "Invalid register address"
    
    let getSCond scond =
        let matchCond cond =
            match cond with
            | "EQ" -> EQ
            | "NE" -> NE
            | "CS" -> CS
            | "HS" -> HS
            | "CC" -> CC
            | "LO" -> LO
            | "MI" -> MI
            | "PL" -> PL
            | "VS" -> VS
            | "VC" -> VC
            | "HI" -> HI
            | "LS" -> LS
            | "GE" -> GE
            | "LT" -> LT
            | "GT" -> GT
            | "LE" -> LE
            | "" | "AL" -> AL
            | _  -> failwithf "Invalid condition code"
        
        match scond with
        | Prefix "S" cond -> (UpdateStatus, matchCond cond)
        | cond -> (IgnoreStatus, matchCond cond)
    
    let parseArithmetic (dest, op1, op2, shiftop) : ArithmeticOperandsPattern =
        let destOp = getRegIndex dest
        let op1Op = getRegIndex op1
        let op2Op : ExecOperand = 
           if isNumeric op2 then
               MixedOp (Literal (int op2))
           else
               MixedOp (Register (getRegIndex op2))
        
        (destOp, op1Op, op2Op)


    let parseShift (dest, op1, op2) : ShiftOperandsPattern =
        let destOp = getRegIndex dest
        let op1Op = getRegIndex op1
        let op2Op : MixedOperand = 
           if isNumeric op2 then
               Literal (int op2)
           else
               Register (getRegIndex op2)
        
        (destOp, op1Op, op2Op)
    
    let parseCompare (dest, op1) : CompareOperandsPattern =
        let destOp = getRegIndex dest
        let op1Op : ExecOperand = 
           if isNumeric op1 then
               MixedOp (Literal (int op1))
           else
               MixedOp (Register (getRegIndex op1))
        
        (destOp, op1Op)


    match lsplit with
    | [] -> None

    // ARITHMETIC instructions
    | Prefix "ADD" scond :: dest :: op1 :: op2 :: shiftop ->
        let aoper : ArithmeticOperation = repack (ADD, (getSCond scond))
        let oprand : ArithmeticOperandsPattern = parseArithmetic(dest, op1, op2, shiftop)
        Some (ArithmeticInstruction (aoper, oprand))

    | Prefix "SUB" scond :: dest :: op1 :: op2 :: shiftop ->
        let aoper : ArithmeticOperation = repack (SUB, (getSCond scond))
        let oprand : ArithmeticOperandsPattern = parseArithmetic(dest, op1, op2, shiftop)
        Some (ArithmeticInstruction (aoper, oprand))
    
    | Prefix "ADC" scond :: dest :: op1 :: op2 :: shiftop ->
        let aoper : ArithmeticOperation = repack (ADC, (getSCond scond))
        let oprand : ArithmeticOperandsPattern = parseArithmetic(dest, op1, op2, shiftop)
        Some (ArithmeticInstruction (aoper, oprand))
    
    | Prefix "SBC" scond :: dest :: op1 :: op2 :: shiftop ->
        let aoper : ArithmeticOperation = repack (SBC, (getSCond scond))
        let oprand : ArithmeticOperandsPattern = parseArithmetic(dest, op1, op2, shiftop)
        Some (ArithmeticInstruction (aoper, oprand))
    
    | Prefix "RSC" scond :: dest :: op1 :: op2 :: shiftop ->
        let aoper : ArithmeticOperation = repack (RSC, (getSCond scond))
        let oprand : ArithmeticOperandsPattern = parseArithmetic(dest, op1, op2, shiftop)
        Some (ArithmeticInstruction (aoper, oprand))
    
    // SHIFT instructions
    | Prefix "LSL" scond :: dest :: op1 :: op2 :: shiftop ->
        //do something if shiftop is not null
        let soper : ShiftOperation = repack (LSL, (getSCond scond))
        let oprand : ShiftOperandsPattern = parseShift(dest, op1, op2)
        Some (ShiftInstruction (soper, oprand))

    | Prefix "LSR" scond :: dest :: op1 :: op2 :: shiftop ->
        //do something if shiftop is not null
        let soper : ShiftOperation = repack (LSR, (getSCond scond))
        let oprand : ShiftOperandsPattern = parseShift(dest, op1, op2)
        Some (ShiftInstruction (soper, oprand))

    | Prefix "ASR" scond :: dest :: op1 :: op2 :: shiftop ->
        //do something if shiftop is not null
        let soper : ShiftOperation = repack (ASR, (getSCond scond))
        let oprand : ShiftOperandsPattern = parseShift(dest, op1, op2)
        Some (ShiftInstruction (soper, oprand))

    | Prefix "ROR" scond :: dest :: op1 :: op2 :: shiftop ->
        //do something if shiftop is not null
        let soper : ShiftOperation = repack (ROR, (getSCond scond))
        let oprand : ShiftOperandsPattern = parseShift(dest, op1, op2)
        Some (ShiftInstruction (soper, oprand))

    | Prefix "RRX" scond :: dest :: op1 :: op2 :: shiftop ->
        //do something if shiftop is not null
        let soper : ShiftOperation = repack (RRX, (getSCond scond))
        let oprand : ShiftOperandsPattern = parseShift(dest, op1, op2)
        Some (ShiftInstruction (soper, oprand))
    
    // COMPARE instructions
    | Prefix "CMP" scond :: dest :: op1 :: shiftop ->
        //do something if shiftop is not null
        let cSfx = snd (getSCond scond)
        let soper = (CMP, cSfx)
        let oprand = parseCompare(dest, op1)
        Some (CompareInstruction (soper, oprand))
    
    | Prefix "CMN" scond :: dest :: op1 :: shiftop ->
        //do something if shiftop is not null
        let cSfx = snd (getSCond scond)
        let soper = (CMN, cSfx)
        let oprand = parseCompare(dest, op1)
        Some (CompareInstruction (soper, oprand))
    
    | Prefix "TST" scond :: dest :: op1 :: shiftop ->
         //do something if shiftop is not null
         let cSfx = snd (getSCond scond)
         let soper = (TST, cSfx)
         let oprand = parseCompare(dest, op1)
         Some (CompareInstruction (soper, oprand))

    | Prefix "TEQ" scond :: dest :: op1 :: shiftop ->
         //do something if shiftop is not null
         let cSfx = snd (getSCond scond)
         let soper = (TEQ, cSfx)
         let oprand = parseCompare(dest, op1)
         Some (CompareInstruction (soper, oprand))

     // BITWISE instructions
     | Prefix "AND" scond :: dest :: op1 :: op2 :: shiftop ->
         let aoper = repack (AND, (getSCond scond))
         let oprand : ArithmeticOperandsPattern = parseArithmetic(dest, op1, op2, shiftop)
         Some (BitwiseInstruction (aoper, oprand))

     | Prefix "EOR" scond :: dest :: op1 :: op2 :: shiftop ->
         let aoper = repack (EOR, (getSCond scond))
         let oprand : ArithmeticOperandsPattern = parseArithmetic(dest, op1, op2, shiftop)
         Some (BitwiseInstruction (aoper, oprand))

     | Prefix "BIC" scond :: dest :: op1 :: op2 :: shiftop ->
          let aoper = repack (BIC, (getSCond scond))
          let oprand : ArithmeticOperandsPattern = parseArithmetic(dest, op1, op2, shiftop)
          Some (BitwiseInstruction (aoper, oprand))

     | Prefix "ORR" scond :: dest :: op1 :: op2 :: shiftop ->
         let aoper = repack (ORR, (getSCond scond))
         let oprand : ArithmeticOperandsPattern = parseArithmetic(dest, op1, op2, shiftop)
         Some (BitwiseInstruction (aoper, oprand))

    | _ -> failwithf "Instruction not implemented"

   
let ParseText (lines: string) = 
    let splitter (s:string) =
        s.Split([|'\n';'\r';|], System.StringSplitOptions.RemoveEmptyEntries)
    splitter lines
        |> Array.toList
        |> List.map ParseLine
 