module CommonParserFunctions
open InstructionsCommonTypes
open Machine
open InstructionsCommonTypes
open ErrorHandler
//let fstL l = match l with | x::xn -> x | x -> x

// Remove comments from end of line
let decomment (s:string) =
    match s.Split([|';'; '@'|], 2) with
    | [|x; y|] -> x     //y is the comment
    | [|x|] -> x
    | _ -> ""

// Removes whitespace 
let trimmer (s:string) = s.Trim().ToUpper()

//let Trim s = s.Trim()

// Split the instruction from the rest (params)
let splitInstr (s:string) : (string * string) =
        let parts = s.Split([|' '|], 2, System.StringSplitOptions.RemoveEmptyEntries)
        match parts with
        | [|instr; prms|] -> instr, prms
        | _ -> failwith ("Unrecognized instruction: " + s)
    
    
// Separate params
let splitOperands (s:string) =
    s.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

// Matches instruction word regardless of the condition codes that follow
let (|Prefix|   _|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some (s.Substring(p.Length))
    else
        None

let isNumeric a = fst (System.Int32.TryParse(a))

let parseLiteral a =

    let hexStringToInt (str: string) =
        let inline getIntVal c = (int c &&& 15) + (int c >>> 6) * 9
        let ToCharList (s:string) = [for c in s -> c]
        let hexer = List.fold (fun acc digit -> acc * 16 + digit) 0
        //printfn "hex2string in progress"
        hexer (List.map getIntVal (str |> ToCharList))
        //let mutable n = 0
        //for c in str do
        //    n <- n*16 + hex2int c
        //n    
    
    //printfn "%A" a

    match a with
    | Prefix "&" rest | Prefix "0X" rest -> hexStringToInt rest
    | s -> s |> int



let getRegIndex (r:string) =
    //printfn "gettinc reg index for:%A:" r
    match r.Trim() with
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
    // These are registers but not addressable by user 
    | "CSPR" | "LR" | "PC" | _ -> failwith "Invalid register address"

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
        | "" | "AL" ->  AL
        | _  -> failwithf "Invalid condition code" // + cond
    
    //let maybeUpdate statusFlag cond =
    //    match matchCond cond wit
    //    | Some x -> Some (statusFlag, x)
    //    | None -> None

    match scond with
    | Prefix "S" cond -> UpdateStatus, matchCond cond
    | cond -> IgnoreStatus, matchCond cond

type ALUInstructionType =
    | ArithmeticInstructionT of Arithmetic
    | ShiftInstructionT of Shift
    | CompareInstructionT of Compare
    | BitwiseInstructionT of Bitwise

let getALUInstruction instruction =
        match instruction with
        | "ADD" -> Success (ArithmeticInstructionT ADD)
        | "SUB" -> Success (ArithmeticInstructionT SUB)
        | "ADC" -> Success (ArithmeticInstructionT ADC)
        | "SBC" -> Success (ArithmeticInstructionT SBC)
        | "RSC" -> Success (ArithmeticInstructionT RSC)
        | "LSL" -> Success (ShiftInstructionT LSL)
        | "LSR" -> Success (ShiftInstructionT LSR)
        | "ASR" -> Success (ShiftInstructionT ASR)
        | "ROR" -> Success (ShiftInstructionT ROR)
        | "RRX" -> Success (ShiftInstructionT RRX)
        | "CMP" -> Success (CompareInstructionT CMP)
        | "CMN" -> Success (CompareInstructionT CMN)
        | "TST" -> Success (CompareInstructionT TST)
        | "TEQ" -> Success (CompareInstructionT TEQ)
        | "AND" -> Success (BitwiseInstructionT AND)
        | "EOR" -> Success (BitwiseInstructionT EOR)
        | "BIC" -> Success (BitwiseInstructionT BIC)
        | "ORR" -> Success (BitwiseInstructionT ORR)
        | _ -> Error <| ParseError ("Unrecognized ALU instruction " + instruction)

(*
LSL #imm5 LSR #imm5 ASR #imm5 ROR #imm5 RRX
LSL reg LSR reg ASR reg ROR reg
shift left 0 to 31
logical shift right 1 to 32 arithmetic shift right 1 to 32 rotate right 1 to 31
rotate carry bit into top bit
shift left by register
logical shift right by register arithmetic shift right by register rotate right by register
*)

let parseShift shift =
    match shift with
    | "LSL" -> LSL
    | "LSR" -> LSR
    | "ASR" -> ASR
    | "ROR" -> ROR
    | "RRX" -> RRX
    | _ ->  //printfn "unable to parse shift instr"
            failwith ("Unable to parse shift instruction: " + shift)

let parseMixedOp (op: string) =
    //printfn "parsing mixed op %A" op
    match op.Trim() with
    | Prefix "#" rest -> Literal (parseLiteral rest)
    | reg -> Register (reg |> getRegIndex)       

let parseExecOperand (op: string) (rest: string list) =
    let trimmed (s:string) = s.Trim()

    
    let parseRest r restreg =
        //printfn "parsing the rest %A " (r, restreg)
        let shiftOp = parseShift (r |> trimmed).[..2]
        let mOp = parseMixedOp (r |> trimmed).[3..]
        ExprOp <| (restreg, shiftOp, mOp)

    //printfn "trying to parse exec operand"
    match rest with
    | [] -> //printfn "parsing empty rest"
            MixedOp <| (parseMixedOp op)
    | x::xn -> //printfn "parsing full rest"
               parseRest x (op |> getRegIndex)



    //printfn "tryint to parse op %A" op



/////////////////////////////////////////////
//let splitter (s:string) = s.Split([|','; ' '; '\n'; '\n'; '\r'; '\f'|], 
//                            System.StringSplitOptions.RemoveEmptyEntries)



//let lsplit line : string list = splitter line |> Array.toList

//let repack (a, (b, c)) = (a, b, c)
