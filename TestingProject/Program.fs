namespace VisualInterface

module Program=

    open VisualInterface
    open Expecto
    open Execution
    open Machine
    open InstructionsCommonTypes
    open Memory

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
            MemLocs = [0x100;0x104;0x108;0x10C;0x110;0xff000000;0xfefffffc;0xfefffff8;0xfefffff4;0xff000004;0xff000008;0xfffffc]                    // memory locations to be traced and data returned

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
//        printfn "flagsint=%x, trace=%A" flagsInt trace.[5]
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
        testCase name <| fun p1 ->
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

    
    let fsConfig = { FsCheck.Config.Default with MaxTest = 10 ; QuietOnSuccess=false}   
    let seqConfig = { Expecto.Tests.defaultConfig with parallel = false; parallelWorkers=1}
    

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

    let enumerateSimpleCases<'T> =
        let cases = FSharp.Reflection.FSharpType.GetUnionCases(typeof<'T>)
        cases |> Array.map (fun c -> (Reflection.FSharpValue.MakeUnion(c,[||]) :?> 'T), c.Name)

    let constructLookup (x:'T) =
        Map.ofArray (enumerateSimpleCases<'T>)

    let arithmeticToStr = Map.ofArray enumerateSimpleCases<Arithmetic>
    let moveToStr = Map.ofArray enumerateSimpleCases<Move>
    let singleMemToStr = Map.ofArray enumerateSimpleCases<SingleMemory>
    let multipleMemToStr = Map.ofArray enumerateSimpleCases<MultipleMemory>
    let memAddressToStr = Map.ofArray enumerateSimpleCases<MemoryAddress>

    let shiftToStr = Map.ofArray enumerateSimpleCases<Shift>
    let compareToStr = Map.ofArray enumerateSimpleCases<Compare>
    let bitwiseToStr = Map.ofArray enumerateSimpleCases<Bitwise>
    let stackDirectionToStr = Map.ofArray enumerateSimpleCases<StackDirection>

    let memoryModeToStr = Map.ofArray [|(Byte, "B"); (Word, "")|]
    let setBitToStr = Map.ofArray [|(UpdateStatus, "S"); (IgnoreStatus, "")|]
    let condCodeToStr = Map.ofArray enumerateSimpleCases<ConditionSuffix>
    let registerToStr = Map.ofArray enumerateSimpleCases<RegisterIndex>

    //need to change this to take instuctions strings for visual and array of instr for our code
    let runSimulators instr =
        let visualResult = (RunVisualWithFlagsOutLocs [0;1;2;3] instr)


        let ourResultMachineState = Instruction.prepareState instr |> Instruction.runAll

        let ourRegisters = State.getRegisters ourResultMachineState |> Map.toList |> List.map (fun (a,b) -> mapToVisualReg a, b )
        let ourFlags = mapToVisualFlags <| State.getStatus ourResultMachineState

        let visualFlags, visualRegisters = match visualResult with | (f, r) -> (f, List.map (fun (a,b) -> (a,b)) (Map.toList r))

//        let visualRegisters = State.getRegisters ourResultMachineState |> Map.toList |> List.map (fun (a,b) -> mapToVisualReg a, b )
//        let visualFlags = mapToVisualFlags <| State.getStatus ourResultMachineState

        ourFlags, ourRegisters, visualFlags, visualRegisters

    let runSimulatorsWOParser vInstr cInstr =
        let visualResult = (RunVisualWithFlagsOutLocs [0x100;0x104;0x108;0x10C;0x110] vInstr)
        let instrs,labels,specialInstrs = cInstr
        let ourResultMachineState = Instruction.prepareStateWOParser instrs labels specialInstrs  |> Instruction.runAll

        let ourRegisters = State.getRegisters ourResultMachineState |> Map.toList |> List.map (fun (a,b) -> mapToVisualReg a, b )
        let ourFlags = mapToVisualFlags <| State.getStatus ourResultMachineState

        let visualFlags, visualRegisters = match visualResult with | (f, r) -> (f, List.map (fun (a,b) -> (a,b)) (Map.toList r))

//        let visualRegisters = State.getRegisters ourResultMachineState |> Map.toList |> List.map (fun (a,b) -> mapToVisualReg a, b )
//        let visualFlags = mapToVisualFlags <| State.getStatus ourResultMachineState

        ourFlags, ourRegisters, visualFlags, visualRegisters

    let generateFlagsInstructions (carry:bool) (zero:bool) (negative:bool) (overflow:bool) =
        let genCarryInstr = "MOV R0, #1\nMOV R1, #0\nCMP R0, R1\n"
        let genOverflowInstr = "MOV R0, #0xF0000000\nMOV R1, #0x7FFFFFFF\nCMP R0, R1\n"
        let instr =
            if zero then "MOVS R1, #0\n" else ""
            +
            if negative then "MOVS R1, #-1\n" else ""
            +
            if carry then genCarryInstr else ""
            +
            if overflow then genOverflowInstr else ""
        instr

    // Function to get flag instructions without the parser
//    let generateFlagsInstructionsWOParser (carry:bool) (zero:bool) (negative:bool) (overflow:bool) =
//        let genCarryInstr = "MOV R0, #1\nMOV R1, #0\nCMP R0, R1\n"
//        let genCarryInstrWOParser = 
//            [|MemInst <| MemoryInstruction.makeMoveSample MOV IgnoreStatus AL R0 (MixedOp <| Literal  1); 
//            MemInst <| MemoryInstruction.makeMoveSample MOV IgnoreStatus AL R1 (MixedOp <| Literal  0);
//            ALUInst <| ALU.ALUInstruction.constructCompareSample R0 R1 |] 
//        let genOverflowInstr = "MOV R0, #0xF0000000\nMOV R1, #0x7F000000\nCMP R0, R1\n"
//        let genOverflowInstrWOParser =
//            [|MemInst <| MemoryInstruction.makeMoveSample MOV IgnoreStatus AL R0 (MixedOp <| Literal 0xF0000000 ); 
//            MemInst <| MemoryInstruction.makeMoveSample MOV IgnoreStatus AL R1 (MixedOp <| Literal  0x7F000000);
//            ALUInst <| ALU.ALUInstruction.constructCompareSample R0 R1 |]
//        let instr =
//            if carry then genCarryInstr else ""
//            +
//            if zero then "MOVS R1, #0\n" else ""
//            +
//            if negative then "MOVS R1, #-1\n" else ""
//            +
//            if overflow then genOverflowInstr else ""
//        let instrWOParser =
//            let cA = if carry then
//                        genCarryInstrWOParser
//                     else [||]
//            let zA = if zero then
//                        Array.append cA [|MemInst <| MemoryInstruction.makeMoveSample MOV UpdateStatus AL R1 (MixedOp <| Literal  0)|]
//                     else cA
//            let nA = if negative then
//                        Array.append zA [|MemInst <| MemoryInstruction.makeMoveSample MOV UpdateStatus AL R1 (MixedOp <| Literal  -1)|]
//                     else zA
//            let vA = if overflow then
//                                    Array.append cA genOverflowInstrWOParser
//                                else cA
//            vA
//        instr,instrWOParser

    let testMoveSimple pName f p1c = 
        testPropertyWithConfig fsConfig pName
        <| fun (opcode:Move) (setBit:SetBit)
                (op1Val:int16) -> //(n:bool) (z:bool) (c:bool) ->
            //let cInstr,cInstrWO = generateFlagsInstructionsWOParser c z n false
            let visualInstr = (moveToStr.[opcode]) + (setBitToStr.[setBit]) + " R1, #" + (string op1Val)
            let codeInstr = [|MemInst <| MemoryInstruction.makeMoveSample opcode setBit AL R1 (MixedOp (Literal  <|int op1Val))|]
            let l = Map.empty<string,int>
            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulatorsWOParser visualInstr (codeInstr,l,[||])
            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"
    
    let testMemSingle pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (opcode:SingleMemory) (b:MemoryMode) (op1Val:int16) ->
            let initInstr = "Label DCD 5435,235,645,00,-253 \n ADR R0,Label \n"
            let visualInstr = initInstr + "MOV R1,#" + (string op1Val) + "\n" + singleMemToStr.[opcode] + memoryModeToStr.[b] + " R1,[R0]\n"
            let codeInstr = [|MemInst <| MemoryInstruction.makeLoadAddressSample ADR IgnoreStatus AL R0 (Label "Label");
                            MemInst <| MemoryInstruction.makeMoveSample MOV IgnoreStatus AL R1 (MixedOp (Literal  <|int op1Val));
                            MemInst <| MemoryInstruction.makeSingleMemSample opcode b AL R1 R0 Offset None |]
            let specialInstr = [|OInstr <| Other.OtherInstruction.makeDCDSample Other.DCD "Label" [5435;235;645;00;-253] |]
            let l = Map.empty<string,int>
            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulatorsWOParser visualInstr (codeInstr,l,specialInstr)
            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"

    let testMemMultiple pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (d:StackDirection) (op1Val:int16) (op2Val:int16) (op3Val:int16) -> 
           let setupInstr = "MOV R0,#" + (string op1Val) + "\n MOV R1,#" + (string op2Val) + "\n MOV R2,#" + (string op3Val)
           let visualInstr = setupInstr + "\n STM" + stackDirectionToStr.[d] + " R13!,{R0-R2}\n LDM" + stackDirectionToStr.[d] + " R13!,{R3-R5}"
           let codeInstr = [|MemInst <| MemoryInstruction.makeMoveSample MOV IgnoreStatus AL R0 (MixedOp (Literal  <|int op1Val));
                           MemInst <| MemoryInstruction.makeMoveSample MOV IgnoreStatus AL R1 (MixedOp (Literal  <|int op2Val));
                           MemInst <| MemoryInstruction.makeMoveSample MOV IgnoreStatus AL R2 (MixedOp (Literal  <|int op3Val));
                           MemInst <| MemoryInstruction.makeMultipleMemSample STM d AL R13 [R0;R1;R2];
                           MemInst <| MemoryInstruction.makeMultipleMemSample LDM d AL R13 [R3;R4;R5]|]
           let l = Map.empty<string,int>
           let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulatorsWOParser visualInstr (codeInstr,l,[||])
           Expect.equal ourFlags visualFlags "CSPR register differes"
           Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"
    
    let testArithmeticSimple pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (opcode:Arithmetic) (setBit:SetBit)
               (op1Val:int) (op2Val:int)
               (carry:bool) ->
            printf "Running arithmetic tests!\n"
            let setupInstr = "MOV R1, #" + (string op1Val) + "\nMOV R2, #" + (string op2Val) + "\n"
            let instruction = (arithmeticToStr.[opcode]) + (setBitToStr.[setBit]) + " R0, R1, R2"
            let instr = (generateFlagsInstructions carry false false false) + setupInstr + instruction
            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulators instr
            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"

    let testBitwiseSimple pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (opcode:Bitwise) (setBit:SetBit)
//               (condCode:ConditionSuffix)
               (op1Val:int) (op2Val:int)
               (carry:bool) ->
            let setupInstr = "MOV R1, #" + (string op1Val) + "\nMOV R2, #" + (string op2Val) + "\n"
            let instruction = (bitwiseToStr.[opcode]) + (setBitToStr.[setBit]) + " R0, R1, R2"
            let instr = (generateFlagsInstructions carry false false false) + setupInstr + instruction

            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulators instr

            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"

    let testBitwiseFlexible pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (opcode:Bitwise) (setBit:SetBit)
//               (condCode:ConditionSuffix)
               (op1Val:int) (op2Val:int) (shift:Shift) (shiftVal:byte)
               (carry:bool) ->
            let shift' = if shift = RRX then ROR else shift
            let setupInstr = "MOV R1, #" + (string op1Val) + "\nMOV R2, #" + (string op2Val) + "\n"
            let instruction = (bitwiseToStr.[opcode]) + (setBitToStr.[setBit]) + " R0, R1, R2, " + (shiftToStr.[shift']) + " #"+ (string <| shiftVal % 32uy)
            let instr = (generateFlagsInstructions carry false false false) + setupInstr + instruction

            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulators instr

            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"


    let testShiftSimple pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (opcode:Shift) (setBit:SetBit)
//               (condCode:ConditionSuffix)
               (op1Val:int) (op2Val:uint16)
               (carry:bool) ->
            let setupInstr = "MOV R1, #" + (string op1Val) + "\nMOV R2, #" + (string op2Val) + "\n"
            let instruction = (shiftToStr.[opcode]) + (setBitToStr.[setBit]) + " R0, R1, R2"
            let instr = (generateFlagsInstructions carry false false false) + setupInstr + instruction

            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulators instr

            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"


    let testArithmeticFlexible pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (opcode:Arithmetic) (setBit:SetBit)
//               (condCode:ConditionSuffix)
               (op1Val:int) (op2Val:int) (shift:Shift) (shiftVal:byte)
               (carry:bool) ->
            let shift' = if shift = RRX then ROR else shift
            let setupInstr = "MOV R1, #" + (string op1Val) + "\nMOV R2, #" + (string op2Val) + "\n"
            let instruction = (arithmeticToStr.[opcode]) + (setBitToStr.[setBit]) + " R0, R1, R2, " + (shiftToStr.[shift']) + " #"+ (string <| shiftVal % 32uy)
            printf "flex arithmetic: %A\n\n" instruction
            let instr = (generateFlagsInstructions carry false false false) + setupInstr + instruction

            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulators instr

            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"

    let testCompareSimple pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (opcode:Compare)
//               (condCode:ConditionSuffix)
               (op1Val:int) (op2Val:int)
               (carry:bool) (zero:bool) (negative:bool) (overflow:bool) ->
            let setupInstr = "MOV R1, #" + (string op1Val) + "\nMOV R2, #" + (string op2Val) + "\n"
            let instruction = (compareToStr.[opcode]) + " R0, R1, R2"
            let instr = (generateFlagsInstructions carry zero negative overflow) + setupInstr + instruction

            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulators instr

            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"

    let testCompareFlexible pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (opcode:Compare)
//               (condCode:ConditionSuffix)
               (op1Val:int) (op2Val:int) (shift:Shift) (shiftVal:byte)
               (carry:bool) (zero:bool) (negative:bool) (overflow:bool) ->
            let shift' = if shift = RRX then ROR else shift
            let setupInstr = "MOV R1, #" + (string op1Val) + "\nMOV R2, #" + (string op2Val) + "\n"
            let instruction = (compareToStr.[opcode]) + " R1, R2, " + (shiftToStr.[shift']) + " #"+ (string <| shiftVal % 32uy)
            let instr = (generateFlagsInstructions carry zero negative overflow) + setupInstr + instruction

            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulators instr

            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"


    let testConditionCodes pName f p1c =
        testPropertyWithConfig fsConfig pName
        <| fun (condCode:ConditionSuffix)
               (op1Val:int) (op2Val:int)
               (carry:bool) (zero:bool) (negative:bool) (overflow:bool) ->
            let opcode = ADD
            let setupInstr = "MOV R1, #" + (string op1Val) + "\nMOV R2, #" + (string op2Val) + "\n"
            let instruction = (arithmeticToStr.[opcode]) + (condCodeToStr.[condCode]) + " R0, R1, R2"
            let instr = (generateFlagsInstructions carry zero negative overflow) + setupInstr + instruction

            let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulators instr

            Expect.equal ourFlags visualFlags "CSPR register differes"
            Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"


    [<EntryPoint>]
    let main argv = 
        InitCache defaultParas.WorkFileDir // read the currently cached info from disk to speed things up
        printf "%A\n" seqConfig
        let tests = 
            testList "Simulator against Visual" [
//            testCase "Comparisons" <|
//                    fun (condCode:ConditionSuffix)
//                       (op1Val:int) (op2Val:int)
//                       (carry:bool) (zero:bool) (negative:bool) (overflow:bool) ->
//                    let opcode = ADD
//                    let setupInstr = "MOV R1, #" + (string op1Val) + "\nMOV R2, #" + (string op2Val) + "\n"
//                    let instruction = (arithmeticToStr.[opcode]) + (condCodeToStr.[condCode]) + " R0, R1, R2"
//                    let instr = (generateFlagsInstructions carry zero negative overflow) + setupInstr + instruction
//
//                    let ourFlags, ourRegisters, visualFlags, visualRegisters = runSimulators instr
//
//                    Expect.equal ourFlags visualFlags "CSPR register differes"
//                    Expect.sequenceEqual ourRegisters visualRegisters "Registers differ"

                    testMoveSimple "Random move test with constant operand" () ()
                    testMemSingle "Random single memory instruction test" () ()
                    testMemMultiple "Random multiple memory instruction test" () ()
//                  testArithmeticSimple "Random arithmetic tests with constant second operand" () ()
//                  testArithmeticFlexible "Random arithmetic tests with flexible second operand" () ()
//                  testBitwiseSimple "Random logic tests with constant second operand" () ()
//                  testBitwiseFlexible "Random logic tests with flexible second operand" () ()
//                  testShiftSimple "Random shift tests" () ()
//                  testCompareSimple "Compare opcodes tests with random values in registers" () ()
//                  testCompareFlexible "Compare opcode tests with flexible second operand" () ()
//                  testConditionCodes "Random condition code tests with ADD" () ()


            ]
//        let rc = 0
//        let rc = runTests seqConfig <| testArithmeticSimple "Random arithmetic tests with constant second operand" () ()
        let rc = runTests seqConfig <| tests
        System.Console.ReadKey() |> ignore
        rc
//        rc // return an integer exit code - 0 if all tests pass
