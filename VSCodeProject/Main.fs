module Main

open Machine
open Instruction
open Execution


// let state = {Register = initialRegisters;Memory = initialMemory}
let state = State.makeInitialState()

// list of instructions with type Instruction (from exec file)

let rec runProgram instructionsList (state: State) =
    match instructionsList with
    | h :: t -> runProgram t (executeALUInstruction state h)
    | _ -> state



[<EntryPoint>]

let main argv =
    let operation1: ArithmeticOperation = ADD, UpdateStatus, AL
    // let operation = IAR operation'
    // let operands = ArithmeticOperands (R0, R1, MixedOp <| Literal 55)
    let instruction = ArithmeticInstruction (operation1, (R0, R1, MixedOp <| Literal 55))
    let newState = executeALUInstruction state instruction
    // let ss = newState >> newState >> newState >> newState <| state
    // printf "Registers: %A\n" <| State.registerValue R0 ss

    let instructionList = List.map Option.get <| Parser.ParseText("")
    let newState = runProgram instructionList state
    
    // printf "Completed %A" k
    System.Console.ReadKey() |> ignore
    // let stateTransform = "abc" |> Parser.ParseLine |> Option.get |> executeALUInstruction state


    0

