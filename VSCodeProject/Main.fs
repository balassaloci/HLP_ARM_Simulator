module Main

open Machine
open Instruction
open Execution


// let state = {Register = initialRegisters;Memory = initialMemory}
let state = State.makeInitialState()

// list of instructions with type Instruction (from exec file)

// let rec runProgram instructionsList (state: State) =
//     match instructionsList with
//     | h :: t -> runProgram t (executeInstruction h state)
//     | _ -> state



[<EntryPoint>]

let main argv =
    let operation1: ArithmeticOperation = ADD, UpdateStatus, AL
    // let operation = IAR operation'
    // let operands = ArithmeticOperands (R0, R1, MixedOp <| Literal 55)
    let instruction = ArithmeticInstruction (operation1, (R0, R1, MixedOp <| Literal 55))
    let newState = executeInstruction state instruction

    // printf "Registers: %A\n" <| State.registerValue R0 newState

    let k = Parser.ParseLine("CMN R0 R1")

    printf "Completed %A" k

    System.Console.ReadKey() |> ignore


    0

