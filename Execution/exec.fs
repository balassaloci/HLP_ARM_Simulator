type RegisterIndex = | R0 | R1 | R2
type MachineState = | INil

// type IOperand = | Register of RegisterIndex | Literal of int
type IOperand = | Register of RegisterIndex | Literal of int
type ICommand = | ADD | SUB | LSL
type SimpleList = IOperand array
type ListWithShift = SimpleList * ICommand * IOperand
type Instruction = | SimpleInstruction of ICommand * SimpleList
                   | ComplexInstruction of ICommand * ListWithShift

let add (op1:int) (op2:int) = op1 + op2

let sub op1 op2 = op1 - op2

let lshift op sz = op <<< sz

let mutable memory = Map []

let fetchVal (R:RegisterIndex) = 
    memory.[R]

let assignVal (R:RegisterIndex) (value:int) =
    memory <- Map.add R value memory

let (|OperandValue|_|) = function
    | Register(R) -> Some <| fetchVal R
    | Literal(x) -> Some x

let (|MatchSimpleOps|_|) = function
    | [|Register dest; OperandValue op1; OperandValue op2 |] -> Some (dest, op1, op2)
    | _ -> failwith "Weird error 3"

let applyInstruction fn op1 op2 =
    match op1, op2 with
        | OperandValue val1, OperandValue val2 -> fn val1 val2
        | _ -> failwith "Weird error 1" 

let evaluateOperands3 = function
    | SimpleInstruction(_, simpleOps) ->
        match simpleOps with
        | MatchSimpleOps(dest, op1, op2) -> (dest, op1, op2)
        // | [|Register dest; OperandValue op1; OperandValue op2 |] ->
            // (dest, op1, op2)
        | _ -> failwith "Eval error"
    | ComplexInstruction(_, complexOps) ->
        match complexOps with
        | (s, shiftCmd, OperandValue shiftSize) -> 
            match s with
            | MatchSimpleOps(d, arg1, arg2) ->
                let arg2' = match shiftCmd with
                            | LSL -> lshift arg2 shiftSize
                            | _ -> failwith "Eval error"
                (d, arg1, arg2')
            | _ -> failwith "Eval error"
        | _ -> failwith "Eval error"
                
// This works only for instructions that have three operands.
// There are 14 of these: arithmetic, shifts, bitwise logic
// For each only need a small function that does computation on input values.
// This is enough to simulate all behaviour without conditions, flags, shifts.
let executeInstruction3 (state:MachineState) (instruction:Instruction) =
    let dest, op1, op2 = evaluateOperands3 instruction
    // let dest, op1, op2 =
    //     match instruction with
    //     | (_, op) -> op.[0], op.[1], op.[2]

    // Want to have Map<instruction, it's definition>
    let command = match instruction with
                  | SimpleInstruction(cmd, _) -> cmd
                  | ComplexInstruction(cmd, _) -> cmd

    let result = 
        match command with
        | ADD -> add op1 op2
        | SUB -> sub op1 op2

    assignVal dest result
    state
 

let operands = [| Register R0; Literal 3; Literal 2 |]
let operands2 = [| Register R1; Literal 3; Literal 2 |]
let instruction = SimpleInstruction (ADD, operands)
let instruction2 = ComplexInstruction (ADD, (operands2, LSL, Literal 5))
// executeInstruction3 INil instruction

Map.toArray memory