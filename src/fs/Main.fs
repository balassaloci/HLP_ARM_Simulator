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

type Buttonstate = BRunAll | BRunStep | BReset | BResetUnsuccessful

/// wrapped in model since maybe more elements will follow
type Model =
    {
        MachineState: State
        Buttons: Buttonstate list
    }

/// actions that are called from elments in the view
type Actions =
    | HighlightLine of int
    | Run of string
    | Reset
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

// TODO: ability to reset machinestate
//       error handling!!
let update model msg =

    let optionGetter = function
        | Some x -> x
        | None -> failwithf "no!"

    let model' =
        match msg with
        | Run s | RunOne s -> let processOneLine =
                                s |> ParseText |> List.map optionGetter |> executeALUInstructionList model.MachineState
                              {model with MachineState = processOneLine}
        | Reset -> let defaultButtonState = [BRunAll; BRunStep];
                   {model with Buttons = defaultButtonState}        // TODO: reset machine state
        | HighlightLine line ->  let doc = cmEditor().getDoc()
                                 doc.addLineClass(line,"background", "line-bg") |> ignore
                                 model // return unaltered model, as this is only a sideeffect
                                       // can probably be done in jsCall below TODO
                             
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

let buttonOnClick label cl func =
    li  []
        [
            p [attribute "class" "navbar-btn"]
                [
                a [ attribute "type" "button"
                    attribute "class" ("btn btn-" + cl)
                    onMouseClick func]
                    [ text label] 
                ]
        ]


let runButton = function
    | BRunAll -> buttonOnClick "Run" "default" fetchAndRun
    | BRunStep -> buttonOnClick "Run Step" "default" fetchAndRunOne
    | BReset -> buttonOnClick "Reset" "success" (fun x -> Reset)
    | BResetUnsuccessful -> buttonOnClick "Reset" "danger" (fun x -> Reset)


let message msg =
    div [attribute "class" "alert alert-warning alert-dismissible alert-head"
         attribute "role" "alert"]
        //  Popover by activating the following and then message.popover('show')
        //  attribute "data-toggle" "popover"
        //  attribute "data-trigger" "focus" 
        //  attribute "data-placement" "bottom" 
        //  attribute "data-content" "Vivamus sagittis lacus vel augue laoreet rutrum faucibus."]
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
                                (model.Buttons |> List.map runButton)
                            (message "alert")
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

let oneRegisterHorizontalWrapper (name, value) =
    li [attribute "class" "col-cprs"]
        [
            text value
            span [attribute "class" "badge"]
                 [text name]
        ]

let cprsView =
    ul [attribute "class" "list-group row-cprs"]
        [
            oneRegisterHorizontalWrapper("C", "1");
            oneRegisterHorizontalWrapper("P", "1");
            oneRegisterHorizontalWrapper("R", "1");
            oneRegisterHorizontalWrapper("S", "1");                        
        ]
// <div class="row">
//   <div class="col-md-6">
//     <div class="list-group-item">0<span class="badge">R0</span>
//   </div>
// </div>
//     <div class="col-md-6">
//         <div class="list-group-item">0<span class="badge">R0</span></div>
//     </div>
// </div>
let body model =
    div [attribute "class" "container starter-template"]
        [
            div [attribute "class" "row"]
                [
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
                            cprsView 
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
    let initButtons = [BRunAll; BRunStep]
    let initModel = {MachineState = initMachineState; Buttons = initButtons}

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

    cmEditor().setValue "ADD R0, R0, R1"
//     cmEditor().setValue "        MOV R0, #1024          ; R0 is input, decreases by factors of 10
//         MOV R1, #0             ; R1 is sum of digits
//         MOV R2, #0x19000000    ; R2 is constantly 0x1999999A
//         ORR R2, R2, #0x00990000
//         ORR R2, R2, #0x00009900
//         ORR R2, R2, #0x0000009A
//         MOV R3, #10            ; R3 is constantly 10
// loop    UMULL R4, R5, R0, R2   ; R5 is R0 / 10
//         UMULL R4, R6, R5, R3   ; R4 is now 10 * (R0 / 10)
//         SUB R4, R0, R4         ; R5 is now one's digit of R0
//         ADD R1, R1, R4         ; add it into R1
//         MOVS R0, R5
//         BNE loop"
        
    // let doc = cmEditor().getDoc()
    // printf "%A" (doc.addLineClass(0,"background", "line-bg"))
    // printf "%A" (cmEditor().setLineClass (1.0, "fg", "line-bg"))
    
main()

