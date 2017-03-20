namespace VisualInterface

module Program=

    open VisualInterface
    open Expecto
    open Execution
    open Machine

    /// postlude which sets R1 bits to status bit values
    let NZCVToR12 =
       """
          MOV R1, #0
          ADDMI R1, R1, #8
          ADDEQ R1, R1, #4
          ADDCS R1, R1, #2
          ADDVS R1, R1, #1
       """ 

    let defaultParas = {
            Cached = false                  // true if results are stored in a cache on disk and reused to speed 
                                            // up future repeat simulations
            VisualPath =  @"..\..\..\visualapp\visual\" // the directory in which the downloaded VisUAL.exe can be found
            WorkFileDir = @"..\..\..\VisualWork\"      // the directory in which both temporary files and the persistent cache file are put
            MemDataStart = 0x100            // start of VisUAL data section Memory
            MemLocs = []                    // memory locations to be traced and data returned

        }

    type Flags = 
        {
            FN: bool
            FZ: bool
            FC: bool
            FV: bool
        }
   
    let defParasWithLocs locs = {defaultParas with MemLocs = locs}
    
    /// Adds postlude to assembly code to detect flags values.
    /// Returns registers (before flag detection code) * flags
    let RunVisualWithFlagsOut paras src =
        let asm = src + NZCVToR12
        let trace = VisualInterface.RunVisual defaultParas asm
        if Array.length trace < 5 then failwithf "Error: Trace \n%A\nfrom\n%s\n has length %d < 5." trace asm (Array.length trace)
        let regs = 
            [0..15] 
            |> List.map (fun n -> R n, trace.[5].ResOut.[R n]) // get reg values before postlude
            |> Map.ofList
        let flagsInt = trace.[0].ResOut.[R 1] //Postlude code sets R1(3:0) equal to NZCV
        printfn "flagsint=%x, trace=%A" flagsInt trace.[5]
        let flagBool n = (flagsInt &&& (1 <<< n)) > 0
        { 
          FN = flagBool 3
          FZ = flagBool 2
          FC = flagBool 1
          FV = flagBool 0
        }, regs

    /// Run Visual with specified source code and list of memory locations to trace
    /// src - source code
    /// memLocs - list of memory locations to trace
    let RunVisualWithFlagsOutLocs memLocs src =
        RunVisualWithFlagsOut {defaultParas with MemLocs = memLocs} src

    /// convenience function, convert 4 char string to NZCV status flag record
    let strToFlags s =
        let toBool = function | '0' -> false | '1' -> true | s -> failwithf "Bad character in flag specification '%c'" s
        match s |> Seq.toList |> List.map toBool with
        | [ a ; b ; c ; d] -> { FN=a; FZ=b;FC=c;FV=d}
        | _ -> failwithf "Wrong number of characters (should be 4) in flag specification %s" s
    
    
    /// run an expecto test of VisUAL
    /// name - name of test
    ///
    let VisualUnitTest name src (flagsExpected:string) (outExpected: (Out * int) list) =
        testCase name <| fun () ->
            let mems = outExpected |> List.collect (function | Mem n, x -> [n,x] | _ -> [])
            let memLocs = mems |> List.map fst
            let flags, outs = RunVisualWithFlagsOutLocs memLocs src
            Expecto.Expect.equal flags (flagsExpected |> strToFlags) "Status flags don't match"
            let regs = outExpected |> List.filter (function | R _,_ -> true | _ -> false)
            let getOut (out, v) = 
                try
                    out, outs.[out]
                with
                | _ -> failwithf "Can't find output %A in outs %A" out outs
            Expecto.Expect.sequenceEqual (outExpected |> List.map getOut) outExpected "Reg and Mem outputs don't match"

    
          
    let seqConfig = { Expecto.Tests.defaultConfig with parallel = false}

    let mapToVisualReg = function
        | R0 -> R 0
        | R1 -> R 1
        | R2 -> R 2
        | R3 -> R 3
        | R4 -> R 4
        | R5 -> R 5
        | R6 -> R 6
        | R7 -> R 7
        | R8 -> R 8
        | R9 -> R 9
        | R10 -> R 10
        | R11 -> R 11
        | R12 -> R 12
        | R13 -> R 13
        | LR -> R 14
        | PC -> R 15

    let mapToVisualFlags (flags:Map<StatusBit,bool>) =
        {FN=flags.[N]; FZ=flags.[Z];FC=flags.[C];FV=flags.[V]}

    [<EntryPoint>]
    let main argv = 
        InitCache defaultParas.WorkFileDir // read the currently cached info from disk to speed things up
        let tests = 
            testList "Ours against Visual" [
               // VisualUnitTest "SUB test" "SUB R0, R0, #1" "0000" [R 0, -1]
//                VisualUnitTest "SUBS test" "SUBS R0, R0, #0" "0110" [R 0, 0]
//                VisualUnitTest "This ADDS test should fail" "ADDS R0, R0, #4" "0000" [R 0, 4; R 1, 1] // R1 should be 0 but is specified here as 1
                testCase "Check random instructions" <| fun () ->
                    let instructionsBase = "MOV R0, #5\nMOV R1, #6\n"
                    let instructions = instructionsBase + "CMP R0, #5\n"

                    let initialState = Instruction.prepareState instructions
                    let ourResultMachineState = Instruction.runAll initialState
            //        let ourRegisters = State.getRegisters result1 |> Map.toList |> List.map (fun (_,b) -> b)

                    let ourRegisters = State.getRegisters ourResultMachineState |> Map.toList |> List.map (fun (a,b) -> mapToVisualReg a, b )
                    let ourFlags = mapToVisualFlags <| State.getStatus ourResultMachineState

                    let visualResult = (RunVisualWithFlagsOutLocs [0] instructions)
                    let (visualFlags, visualRegisters) = 
                        match visualResult with | (f, r) -> (f, List.map (fun (a,b) -> (a,b)) (Map.toList r))
//                    let correctResult = visualRegisters
//                    let ourResult = ourRegisters
                    
                    Expect.equal ourFlags visualFlags "CSPR register differes"
                    Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"

            ]
        let rc = runTests seqConfig tests
//        let instructions = "MOV R0, #1\nMOV R1, #6\nLSRS R0, R0, #1 \nSUBS R2, R0, R1"
//        let instructionsBase = "MOV R0, #5\nMOV R1, #6\n"
//        let instructions = instructionsBase + "CMP R0, #5"
//
//        let initialState = Instruction.prepareState instructions
//        let result1 = Instruction.runAll initialState
////        let ourRegisters = State.getRegisters result1 |> Map.toList |> List.map (fun (_,b) -> b)
//        let ourRegisters = State.getRegisters result1 |> Map.toList |> List.map (fun (a,b) -> mapToVisualReg a, b )
//        let ourFlags = State.getStatus result1
//        let result = (RunVisualWithFlagsOutLocs [0] instructions)
//        let (flags, registers) = 
//            match result with
//            | (f, r) -> (f, List.map (fun (a,b) -> (a,b)) (Map.toList r))
//        printf "\n\n\n\n%A\n" flags
//        printf "\n\n\n\n%A\n" [0..15]
//        printf "%A\n" registers
//        printf "%A\n" ourRegisters
//        printf "%A\n" ourFlags
        System.Console.ReadKey() |> ignore
        rc
//        rc // return an integer exit code - 0 if all tests pass
