module ExecutionTests
open Expecto

type DTest = A | B | C

let enumerateSimpleCases<'T> =
    let cases = FSharp.Reflection.FSharpType.GetUnionCases(typeof<'T>)
    cases |> Array.map (fun c -> Reflection.FSharpValue.MakeUnion(c,[||]) :?> 'T)

let values = enumerateSimpleCases<DTest>
let fsConfig = {FsCheckConfig.defaultConfig with maxTest=2000}

[<Tests>]
let tests =
    testList "Basic tests" [
        testCase "Check hello world" <| fun () ->
            let correctSubject = "Hello, World"
            let subject = "Hello, World"
            Expect.equal subject correctSubject "The strings should equal"
        
        testCase "Placeholder test" <| fun () ->
            Expect.equal "one" "one" "one equals one!"
        
        testPropertyWithConfig fsConfig "Addition - commutative" <| fun a b ->
            a + b = b + a
        
        testPropertyWithConfig fsConfig "Product is distributive over addition" <| fun a b c ->
            a * (b + c) = a*b + a*c

    ]
[<EntryPoint>]
let main args =
    // runTestsInAssembly defaultConfig [|"--summary"|] 
    runTests defaultConfig tests
