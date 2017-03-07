module App.Main

open Fable.Core
open Fable.Import
open Fable.Import.Browser

open Fable.Arch
open Fable.Arch.App
open Fable.Arch.Html

open Fable.Core.JsInterop
open App.CodeMirrorInterface
//open CodeMirrorImports

open Machine
open Parser
open Execution
// union name
open Microsoft.FSharp.Reflection


type Editor = | Uninit
              | CM of CodeMirrorImports.CodeMirrorEditor

type Model =
    {
        MachineState: State
        CM : string
        Code : Editor
    }

type Actions =
    | InitialiseCodeMirror
    | ChangeInput of string
    | Run



// Update
let processSimpleAddition model =
    State.updateRegister R0 5 model.MachineState



let update model msg =
    
    let initEditor =
        let editId = getById<Fable.Import.Browser.HTMLTextAreaElement> "code"
        let cmEditor = App.CodeMirrorImports.CodeMirror.fromTextArea(editId, initOptions)
        
        CM cmEditor

    let optionGetter = function
        | Some x -> x
        | None -> failwithf "no!"

    let model' =
        match msg with
        | InitialiseCodeMirror when model.Code = Uninit -> printfn "updatemodelcall"; { model with Code = initEditor }
        | Run -> match model.Code with
                 | CM c -> printfn "TEXTIS: %s" (c.getValue ());
                           let processOneLine =
                                c.getValue() |> ParseLine |> optionGetter |> executeInstruction model.MachineState
                           {model with MachineState = processOneLine model.MachineState}
                 | _ -> printfn "uninit"; model
        | ChangeInput x -> printfn "UPDATETEXT %s" x; {model with CM = x}
        | _ -> failwithf "not implemented"
    let jsCall =
        match msg with
        | _ -> []

    model', jsCall


// View
let inline onInput x = onEvent "oninput" (fun e -> x (unbox e?target?value)) 

///Returns the case name of the object with union type 'ty.
let GetUnionCaseName (x:RegisterIndex) = 
    match FSharpValue.GetUnionFields(x, typeof<RegisterIndex>) with
    | case, _ -> case.Name  

let listRegister reg =
    let getRegisterNameOrVal b = function
        | (k, _) when b -> GetUnionCaseName(k)
        | (_, v) ->  v.ToString()
    let getRegisterName = getRegisterNameOrVal true reg
    let getRegisterVal = getRegisterNameOrVal false reg

    li [attribute "class" "list-group-item"]
       [
            text (getRegisterName)
            span [attribute "class" "badge"]
                 [text getRegisterVal]
       ]

let view model =
    div [attribute "class" "container starter-template"]
        [
            div [attribute "class" "row"]
                [
                    div [attribute "class" "col-md-8"]
                        [
                            textarea [attribute "name" "code"
                                      attribute "id" "code"
                                      attribute "style" "display: none;"]
                                     [text "ADD R0 R1 R1"]
                        ]
                    div [attribute "class" "col-md-4"]
                        [

                            ul [attribute "class" "list-group"]
                               (State.getAllRegisters model.MachineState |> List.map listRegister)
                            a [attribute "class" "btn btn-default"
                               attribute "href" "#"
                               attribute "role" "button"
                               onMouseClick (fun x -> Run)]
                              [text "Run"]
                        ]
                ]
        ]



// Main function
// Starts MVC first and then initialises CodeMirror

let initProducer push =
    printfn "init"
    push(InitialiseCodeMirror)

let main () =

    printfn "Starting..."
    let initMachineState = State.makeInitialState()

    printfn "Creating state"
    let initModel = {MachineState = initMachineState; CM = ""; Code = Uninit}

    createApp initModel view update Virtualdom.createRender
    |> withStartNodeSelector "#test"
    |> withProducer initProducer
//    |> withSubscriber (fun x -> Fable.Import.Browser.console.log("Event received: ", x))
    |> start
    
main()

