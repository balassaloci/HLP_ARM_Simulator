// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Program

open Machine
open Execution


// let state = {Register = initialRegisters;Memory = initialMemory}
let state = State.makeInitialState()

// list of instructions with type Instruction (from exec file)

let rec runProgram instructionsList (state: State) =
     match instructionsList with
     | h :: t -> runProgram t (executeInstruction state h)
     | _ -> state




[<EntryPoint>]
let main argv = 
    let operation1: ArithmeticOperation = ADD, UpdateStatus, AL
    let operation2: ArithmeticOperation = SUB, UpdateStatus, AL
    let instruction1 = ArithmeticInstruction (operation1, (R0, R1, MixedOp <| Literal 55))
    let instruction2 = ArithmeticInstruction (operation2, (R3, R0, ExprOp <| (R1, LSL, Literal 5)))
    let listInstructions = [instruction1;instruction2]
    //let newState = //executeInstruction state instruction
    let newState = runProgram listInstructions state
    printf "Registers0: %A\n" <| State.registerValue R0 newState
    printf "Registers3: %A\n" <| State.registerValue R3 newState
    System.Console.ReadKey() |> ignore
    0
