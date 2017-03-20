module Main

open Machine
// open Instruction
open Execution
open ALU
open InstructionsCommonTypes


// let state = {Register = initialRegisters;Memory = initialMemory}
//let state = State.makeInitialState()

// list of instructions with type Instruction (from exec file)
//let state = State.makeInitialState [|Instruction.constructSample ()|]

[<EntryPoint>]
let main argv =
    // let operation1: ArithmeticOperation = ADD, UpdateStatus, AL
    // let operation = IAR operation'
    // let operands = ArithmeticOperands (R0, R1, MixedOp <| Literal 55)
    // let instruction = ArithmeticInstruction (operation1, (R0, R1, MixedOp <| Literal 55))
    // let newState = executeALUInstruction state instruction
    // let ss = newState >> newState >> newState >> newState <| state
    // printf "Registers: %A\n" <| State.registerValue R0 ss

    // let instructionList = List.map Option.get <| Parser.ParseText("")
    // let result = executeALUInstructionList state instructionList
    let newInstr =
        "ADD R3, R11, #72"
        |> ALUInstruction.parse
        |> Execution.ALUInst

    let instrArray =
        [|"ADD R0 ,R0, #0x10";
         "ADD R1, R0, R0, LSL #2"|]
    
    
//    let startState = State.makeInitialState instrArray
//    Instruction.run
    let instructions = Array.map (ALUInstruction.parse >> Execution.ALUInst) instrArray
    let startState = State.makeInitialState instructions
    let newState =
            startState
            |> Instruction.run 
            |> Instruction.run

//    let newState = List.fold (fun acc d -> Instruction.run acc) startState instructions


    //let state = State.makeInitialState [| newInstr|]
    // printf "Completed %A" k
    //    let newState = Instruction.run state
    printf "%A " <| State.registerValue R0 newState
    printf "%A " <| State.registerValue R1 newState
    printf "%A" <| State.registerValue R2 newState
    System.Console.ReadKey() |> ignore
    // let stateTransform = "abc" |> Parser.ParseLine |> Option.get |> executeALUInstruction state


    0

