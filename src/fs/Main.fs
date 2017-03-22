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
open ErrorHandler
open Execution
open ALU

// union name
open Microsoft.FSharp.Reflection

type Buttonstate = BRunAll | BRunStep | BReset | BResetUnsuccessful
type IRunState = Init | Running

/// wrapped in model since maybe more elements will follow
type Model =
    {
        MachineState: State<Instruction>
        Buttons: Buttonstate list
        Runstate: IRunState
        ErrorMessage: string
    }

/// actions that are called from elments in the view
type Actions =
    | HighlightLine of int
    | Run of string
    | Reset
    | RunOne of string
    | ErrorMessage of string

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


(****************************************************************
    Update part of MVC 
    -> Called every time the view throws an update action
    -> Returns new model as well as jsCalls that will be executed
*****************************************************************)
let update model msg =

    let newButtons machine button =    
        List.map (fun b -> if (b = button && (State.checkEndExecution machine)) then BReset else b ) model.Buttons

    let run s =
        try
            let initState = Instruction.prepareState s
            let newMachine = Instruction.runAll initState
            {model with MachineState = newMachine; Buttons = (newButtons newMachine BRunAll); Runstate = Init} 
        with
            | CustomException(msg) -> {model with ErrorMessage = msg; Runstate = Init}
    
    let model' =
        match msg with
        | Run s -> run s
        | RunOne s when model.Runstate = Init -> let initState = Instruction.prepareState s
                                                 {model with MachineState = initState; Runstate = Running}
        | RunOne _ -> try 
                        let newMachine = Instruction.runOnce model.MachineState
                
                        {model with MachineState = newMachine; Buttons = newButtons newMachine BRunStep}
                      with
                        | CustomException(msg) -> {model with ErrorMessage = msg; Runstate = Init}
        | Reset -> let defaultButtonState = [BRunAll; BRunStep]
                   let resetState = State.makeInitialState [||] Map.empty<string, int>
                   {model with Buttons = defaultButtonState; MachineState = resetState; Runstate = Init}
        | ErrorMessage msg -> {model with ErrorMessage = msg}
        | _ ->  model 
    
    // handle sideeffects separately
    let jsCall =
        // get the current line number at pc counter
        // TODO remove special instructions DCD/FILL
        let getLineNumber state =
            let pc = (State.registerValue PC state)
            (pc  / 4) - 1 // counter starts at 4

        // remove all highlighten lines
        let removeAllLineClasses doc =
            let removeLineClass (doc: CodeMirrorImports.Doc) line =
                doc.removeLineClass(line,"background", "line-bg") |> ignore;

            let lastline = (int (cmEditor().lineCount())) - 1

            [0..lastline] |> List.iter (removeLineClass doc)

        let doc = cmEditor().getDoc()
        
        match msg with
        | RunOne _ -> let line = getLineNumber model'.MachineState
                      toActionList <| fun x -> (removeAllLineClasses doc;
                                                doc.addLineClass(line,"background", "line-bg") |> ignore)        
        | Reset -> toActionList <| fun x -> removeAllLineClasses doc;
        | HighlightLine line -> let doc = cmEditor().getDoc()
                                toActionList <| fun x -> (doc.addLineClass(line,"background", "line-bg") |> ignore)
        | _ -> []

    // return model and jsCall back to application
    model', jsCall


(****************************************************************
    View part of MVC
    -> This builds the whole HTML / DOM structure of the app
*****************************************************************)

/// creates DOM for Buttons in Nav
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

let fetchAndRun x = 
    // get editorValue
    Run (cmEditor().getValue())

let fetchAndRunOne x = 
    // get editorValue
    RunOne (cmEditor().getValue())

let runButton = function
    | BRunAll -> buttonOnClick "Run" "default" fetchAndRun
    | BRunStep -> buttonOnClick "Run Step" "default" fetchAndRunOne
    | BReset -> buttonOnClick "Reset" "success" (fun x -> Reset)
    | BResetUnsuccessful -> buttonOnClick "Reset" "danger" (fun x -> Reset)

/// DOM for Message popup in Navigation
let message msg =
    div [attribute "class" "dismissalert alert alert-warning alert-dismissible alert-head"
         attribute "role" "alert"
         attribute "id" "alert"
         onMouseClick (fun e -> ErrorMessage "")]
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

let popover message =
    div [attribute "class" "alert popover fade bottom in"
         attribute "role" "alert"
         attribute "data-toggle" "popover"
         attribute "id" "popover811790"
         attribute "style" ("top: 45px; left: 573.125px; display: block;")]
        [
            div [attribute "class" "arrow"
                 attribute "style" "left: 50%;"] []
            h3 [attribute "class" "popover-title" 
                attribute "style" "display: none;"] []
            div [attribute "class" "popover-content"] [text message]
        ]


/// Header DOM
let displayMessage (msg: string) =
    printfn "ARR: %A" (msg.Split('\n',':'))
    match msg.Split('\n') |> Array.toList with
    | "" :: _ -> []
    // | h  when String.length m < 20  -> [message m]
    | h :: tl when String.length h < 50 -> [message h; popover (String.concat "" tl)]
    | _ -> [message "Error"; popover msg]
                        

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
                            div [] (displayMessage model.ErrorMessage)
                        ]
                ]
        ]


let memorywrapper memory =
    let memorytable memory = 

        let memoryline (address, value) =
            tr []
                [
                    th [attribute "scope" "row"] [text address]
                    td [][text "?"]
                    td [][text "?"]
                    td [][text "?"]
                    td [][text "?"]
                    td [][text value]
                ]
    
        let convertAddressValue (name, value) =  name.ToString(), value.ToString()

        memory
        |> Map.toList
        |> List.map(convertAddressValue >> memoryline)


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
                    (memorytable memory)
          ]

let memory mem =

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
                                   (memorywrapper mem)
                                ]
                        ]
                ]
        ]

/// helper function to get the name of the union case
let getUnionCaseName (x) = 
        match FSharpValue.GetUnionFields(x, typeof<'T>) with
        | case, _ -> case.Name  

/// creates DOM for register sidebar
let listRegister machineState =
    
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


/// Control Register DOM
let statusBits machineState = 
                        
    let oneRegisterHorizontalWrapper (name, value) =
        li [attribute "class" "col-cprs"]
            [
                text value
                span [attribute "class" "badge"]
                    [text name]
            ]
    
    let bool2String = function
        | true -> "1"
        | false -> "0"

    let convertSystemBits (name, value) =  getUnionCaseName(name), bool2String(value)

    State.getStatus machineState
        |> Map.toList
        |> List.map (convertSystemBits >> oneRegisterHorizontalWrapper)
        

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
                            (memory (State.getMemory model.MachineState))
                        ]
                    div [attribute "class" "col-md-4"]
                        [
                            ul [attribute "class" "list-group"]
                               (listRegister model.MachineState)
                            ul [attribute "class" "list-group row-cprs"]
                               (statusBits model.MachineState) 
                        ]
                ]
        ]


/// View that combines DOM of header and body
let view model =
    section [attribute "class" "app-wrapper"]
            ([(header model); (body model)])



/// Main function that sets up the application
let main () =

    printfn "Starting..."
    let initMachineState = State.makeInitialState [||] Map.empty<string, int>

    printfn "Creating state"
    let initButtons = [BRunAll; BRunStep]
    let initModel = {MachineState = initMachineState; 
                     Buttons = initButtons; 
                     Runstate = Init; 
                     ErrorMessage = ""}

    createApp initModel view update Virtualdom.createRender
    |> withStartNodeSelector "#app"
    |> start

    let initEditor =
        let editId = getById<Fable.Import.Browser.HTMLTextAreaElement> "code"
        let options = [App.CodeMirrorImports.LineNumbers]
        App.CodeMirrorImports.CodeMirror.fromTextArea(editId, options)

    match editorWrapper with
    | Uninit -> editorWrapper <- CM initEditor
    | _ -> failwithf "not possible"

    cmEditor().setSize ("749", "400")
    cmEditor().setValue "ADD R0, R0, R1
SUB R0, R0, R1
LSL R0, R1, #4
SUB R0, R0, #5"
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

    
main()

