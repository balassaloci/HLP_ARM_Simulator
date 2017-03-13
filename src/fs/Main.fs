module App.Main

open Fable.Core
open Fable.Import
open Fable.Import.Browser

open Fable.Arch
open Fable.Arch.App
open Fable.Arch.Html

open Fable.Core.JsInterop
open App.CodeMirrorInterface

open Machine
open Parser
open Execution

// union name
open Microsoft.FSharp.Reflection



/// wrapped in model since maybe more elements will follow
type Model =
    {
        MachineState: State
    }

/// actions that are called from elments in the view
type Actions =
    | Run of string
    | RunOne of string

/// Global Codemirror
/// initialise codemirror as global variable, but assign it to a div
/// only after the application is started
type Editor = | Uninit
              | CM of CodeMirrorImports.CodeMirrorEditor
let mutable editorWrapper = Uninit
/// after initialisation, editor can be called with this function 
/// instead using pattern matching every time
let cmEditor () = 
    match editorWrapper with
    | CM e -> e
    | _ -> failwithf "editor not initialised yet"


(* 
    Update part of MVC 
    -> Called every time the view throws an update action
    -> Returns new model as well as jsCalls that will be executed
*)
let update model msg =

    let optionGetter = function
        | Some x -> x
        | None -> failwithf "no!"

    let model' =
        match msg with
        | Run s | RunOne s-> let processOneLine =
                                s |> ParseText |> List.map optionGetter |> executeALUInstructionList model.MachineState
                             {model with MachineState = processOneLine}
    let jsCall =
        match msg with
        | _ -> []

    model', jsCall


(* 
    View part of MVC
    -> This builds the whole HTML / DOM structure of the app
*)
//let inline onInput x = onEvent "oninput" (fun e -> x (unbox e?target?value)) 



///Returns the case name of the object with union type 'ty.

let listRegister machineState =
    let getUnionCaseName (x:RegisterIndex) = 
        match FSharpValue.GetUnionFields(x, typeof<RegisterIndex>) with
        | case, _ -> case.Name  

    let oneRegister (name, value) =
        li [attribute "class" "list-group-item"]
           [
                text value
                span [attribute "class" "badge"]
                     [text name]
           ]

    // custom sort function to sort registers right
    // if other registers are present, order does not matter
    let sortRegisters ((name: string), _) =
        match name with
        | str when str.StartsWith("R") ->  str.Substring(1) |> int
        | _ -> 100

    // get the registers, and extract name and value in sorted fashion
    State.getRegisters machineState
    |> Map.toList 
    |> List.map( fun (ri, v) -> getUnionCaseName(ri), v.ToString() )
    |> List.sortBy sortRegisters
    |> List.map oneRegister

let fetchAndRun x = 
    // get editorValue
    Run (cmEditor().getValue())

let fetchAndRunOne x = 
    // get editorValue
    RunOne (cmEditor().getValue())

let buttonOnClick label func =
    li  []
        [
            p [attribute "class" "navbar-btn"]
                [
                a [ attribute "type" "button"
                    attribute "class" "btn btn-default"
                    onMouseClick func]
                    [ text label] 
                ]
        ]

let header model =
    nav [attribute "class" "navbar navbar-inverse navbar-fixed-top"]
        [
            div [attribute "class" "container"]
                [
                    div [attribute "class" "navbar-header"]
                        [
                            button [attribute "type" "button"
                                    attribute "class" "navbar-toggle collapsed"
                                    attribute "data-toggle" "collapse"
                                    attribute "data-target" "#navbar"
                                    attribute "aria-expanded" "false"
                                    attribute "aria-controls" "navbar"]
                                    [
                                        span [attribute "class" "sr-only"]
                                             [text "Toggle navigation"]
                                        span [attribute "class" "icon-bar"]
                                             [text ""]
                                        span [attribute "class" "icon-bar"]
                                             [text ""]
                                        span [attribute "class" "icon-bar"]
                                             [text ""]
                                    ]
                            a [attribute "class" "navbar-brand"]
                              [text "HLP"]
                        ]
                    div [attribute "class" "collapse navbar-collapse"
                         attribute "id" "bs-example-navbar-collapse-1"]
                        [
                            ul  [attribute "class" "nav navbar-nav navbar-right"]
                                [
                                    (buttonOnClick "Run All" fetchAndRun)
                                    (buttonOnClick "Run Step" fetchAndRunOne)
                                ]
                        ]

                ]
        ]


let memorytable =
    table [attribute "class" "table"]
          [
              thead []
                    [
                        tr []
                           [
                               th [][text "Word Address"]
                               th [][text "Byte 3"]
                               th [][text "Byte 2"]
                               th [][text "Byte 1"]
                               th [][text "Byte 0"]
                               th [][text "Word Value"]
                           ]
                    ]
              tbody []
                    [
                        tr []
                           [
                               th [attribute "scope" "row"] [text "0x100"]
                               td [][text "0x0"]
                               td [][text "0x0"]
                               td [][text "0x0"]
                               td [][text "0x0"]
                               td [][text "0"]
                           ]
                    ]
          ]

let memory =

    div [attribute "class" "panel-group"
         attribute "id" "accordion"
         attribute "role" "tablist"
         attribute "aria-multiselectable" "true"]
        [
            div [attribute "class" "panel panel-default"
                 attribute "data-toggle" "collapse"
                 attribute "data-target" "#collapseOne"]
                [
                    div [attribute "class" "panel-heading"
                         attribute "role" "tab"
                         attribute "id" "headingOne"]
                        [
                            h4 [attribute "class" "panel-title"]
                               [text "Memory Content"]

                        ]
                    div [attribute "class" "panel-collapse collapse"
                         attribute "id" "collapseOne"
                         attribute "role" "tabpanel" 
                         attribute "aria-labelledby" "headingOne"]
                        [
                            div [attribute "class" "panel-body"]
                                [ 
                                   text "TODO ..."
                                   (memorytable)
                                ]
                        ]
                ]
        ]

let message msg =
    div [attribute "class" "alert alert-warning alert-dismissible"
         attribute "role" "alert"]
        [
            button [attribute "type" "button"
                    attribute "class" "close"
                    attribute "data-dismiss" "alert"
                    attribute "aria-label" "Close"]
                   [
                       span [attribute "aria-hidden" "true"]
                            [text "x"]
                   ]
            strong []
                   [text msg]
        ]
let body model =
    div [attribute "class" "container starter-template"]
        [
            div [attribute "class" "row"]
                [
                    (message "alert")
                    div [attribute "class" "col-md-8"]
                        [
                            textarea [attribute "name" "code"
                                      attribute "id" "code"
                                      attribute "style" "display: none;"]
                                     [text ""]
                            (memory)
                        ]
                    div [attribute "class" "col-md-4"]
                        [
                            ul [attribute "class" "list-group"]
                               (listRegister model.MachineState)
                        ]
                ]
        ]

// <div class="alert alert-warning alert-dismissible" role="alert">
//   <button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>
//   <strong>Warning!</strong> Better check yourself, you're not looking too good.
// </div>



let view model =
    section [attribute "class" "app-wrapper"]
            ([(header model); (body model)])
// Main function
// Starts MVC first and then initialises CodeMirror


let main () =

    printfn "Starting..."
    let initMachineState = State.makeInitialState()

    printfn "Creating state"
    let initModel = {MachineState = initMachineState}

    createApp initModel view update Virtualdom.createRender
    |> withStartNodeSelector "#app"
//    |> withSubscriber (fun x -> Fable.Import.Browser.console.log("Event received: ", x))
    |> start

    let initEditor =
        let editId = getById<Fable.Import.Browser.HTMLTextAreaElement> "code"
        let options = [App.CodeMirrorImports.LineNumbers]
        App.CodeMirrorImports.CodeMirror.fromTextArea(editId, options)

    match editorWrapper with
    | Uninit -> editorWrapper <- CM initEditor
    | _ -> failwithf "not possible"

    
    cmEditor().setValue "ADD R0 R1 R1"
    
main()

