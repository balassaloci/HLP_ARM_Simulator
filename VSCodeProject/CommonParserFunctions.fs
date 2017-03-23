module CommonParserFunctions
open InstructionsCommonTypes
open Machine
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

// Split the instruction from the rest (params)
let splitInstr (s:string) : (string * string) =
        let parts = s.Split([|' '|], 2, System.StringSplitOptions.RemoveEmptyEntries)
        match parts with
        | [|instr; prms|] -> instr, prms
        | _ -> failc ("Unrecognized instruction: " + s)
    
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

//Returns true if param is a numeric (decimal) string
let isNumeric a = fst (System.Int32.TryParse(a))

//Parsing Hexadecimal or decimal literals
let parseLiteral a =
    let hexStringToInt (str: string) =
        //TODO: Make this /if/ forest nicer by simplifying out statements from r
        let inline getIntVal c = 
            if (c >= '0' && c <= '9') ||
                (c >= 'a' && c <= 'f') ||
                (c >= 'A' && c <= 'F') then
                (int c &&& 15) + (int c >>> 6) * 9
            else failc ("Invalid hexadecimal constant")

        let toCharList (s:string) = [for c in s -> c]
        let hexer = List.fold (fun acc digit -> acc * 16 + digit) 0
        hexer (List.map getIntVal (str |> toCharList))

    match a with
    | Prefix "&" rest | Prefix "0X" rest -> hexStringToInt rest
    | s when isNumeric s -> s |> int
    | _ -> failc ("Unable to parse literal: " + a)


//Parses the register index (or fails)
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
    | "CSPR" | "LR" | "PC" | _ -> failc ("Invalid register: " + r)

let getCond x =
    //printfn "getting cond suffix %A" x
    match x with
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
    | _  -> failc ("Invalid condition code: " + x)
    
let getSCond = function
    | Prefix "S" cond -> UpdateStatus, getCond cond
    | cond -> IgnoreStatus, getCond cond


type ALUInstructionType =
    | ArithmeticInstructionT of Arithmetic
    | ShiftInstructionT of Shift
    | CompareInstructionT of Compare
    | BitwiseInstructionT of Bitwise

let getALUInstruction instruction =
    match instruction with
    | "ADD" -> ArithmeticInstructionT ADD
    | "SUB" -> ArithmeticInstructionT SUB
    | "RSB" -> ArithmeticInstructionT RSB
    | "ADC" -> ArithmeticInstructionT ADC
    | "SBC" -> ArithmeticInstructionT SBC
    | "RSC" -> ArithmeticInstructionT RSC
    | "LSL" -> ShiftInstructionT LSL
    | "LSR" -> ShiftInstructionT LSR
    | "ASR" -> ShiftInstructionT ASR
    | "ROR" -> ShiftInstructionT ROR
    | "RRX" -> ShiftInstructionT RRX
    | "CMP" -> CompareInstructionT CMP
    | "CMN" -> CompareInstructionT CMN
    | "TST" -> CompareInstructionT TST
    | "TEQ" -> CompareInstructionT TEQ
    | "AND" -> BitwiseInstructionT AND
    | "EOR" -> BitwiseInstructionT EOR
    | "BIC" -> BitwiseInstructionT BIC
    | "ORR" -> BitwiseInstructionT ORR
    | _ -> failc ("Unrecognized ALU instruction " + instruction)


let parseShift shift =
    match shift with
    | "LSL" -> LSL
    | "LSR" -> LSR
    | "ASR" -> ASR
    | "ROR" -> ROR
    | "RRX" -> RRX
    | _ ->  failc ("Unable to parse shift instruction: " + shift)

let parseMixedOp (op: string) =
    //printfn "parsing mixed op %A" op
    match op.Trim() with
    | Prefix "#" rest -> Literal (parseLiteral rest)
    | reg -> Register (reg |> getRegIndex)

let parseExecOperand (op: string) (rest: string list) =

    let trimmed (s:string) = s.Trim()
    let parseRest r restreg =
        //printfn "parsign rest"
        let r' = r |> trimmed
        //printfn "r' is %A" r'
        if r'.Length < 3 then

            failc ("Unable to parse shift instruction: " + r' +
                    "\nPossible incorrect use of parameters")
        else 
            //printfn "parsing the rest %A " (r, restreg)
            let shiftOp = parseShift (r |> trimmed).[..2]
            //printfn "parsing mixed op %A " r
            let mOp = parseMixedOp (r |> trimmed).[3..]
            ExprOp <| (restreg, shiftOp, mOp)

    //printfn "trying to parse exec operand"
    match rest with
    | [] ->
        //printfn "parsing empty rest"
        MixedOp <| (parseMixedOp op)
    | x::xn ->
        //printfn "parsing full rest %A" (x, op)
        parseRest x (op |> getRegIndex)



    //printfn "tryint to parse op %A" op



/////////////////////////////////////////////
//let splitter (s:string) = s.Split([|','; ' '; '\n'; '\n'; '\r'; '\f'|], 
//                            System.StringSplitOptions.RemoveEmptyEntries)



//let lsplit line : string list = splitter line |> Array.toList

//let repack (a, (b, c)) = (a, b, c)
