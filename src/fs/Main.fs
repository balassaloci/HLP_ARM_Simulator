module App.Main

open Fable.Core
open Fable.Import
open Fable.Import.Browser

open Fable.Arch
open Fable.Arch.App
open Fable.Arch.Html

open Fable.Core.JsInterop
open App.CodeMirrorInterface

// Model
type Register = 
    {
        Name: string
        IntVal: int
    }

type Model = 
    {
        Text: string
        Registers: Register list
    }

type Actions =
    | ChangeInput of string
    | Run



// Update
let processOneLine model =
    let addOneToFirstRegister =
        model.Registers
        |> List.map (fun r -> 
                            let inc = r.IntVal + 1
                            if r.Name = "r0" then {r with IntVal = inc} else r)

    {model with Registers = addOneToFirstRegister }

let update model msg =
    let model' =
        match msg with
        | ChangeInput str -> {model with Text = str}
        | Run -> processOneLine model
    let jsCall =
        match msg with
        | _ -> []

    model', jsCall


// View
let inline onInput x = onEvent "oninput" (fun e -> x (unbox e?target?value)) 

let listRegister reg =
    li [attribute "class" "list-group-item"]
       [
            text (reg.IntVal.ToString())
            span [attribute "class" "badge"]
                 [text reg.Name]
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
                                     [text "abc //comment"]
                        ]
                    div [attribute "class" "col-md-4"]
                        [

                            ul [attribute "class" "list-group"]
                               (model.Registers |> List.map listRegister)
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
// TODO: Integration of CodeMirror into model
let main () =

    printfn "Starting..."

    let initReg = [{Name = "r0"; IntVal = 0}; {Name = "r1"; IntVal = 0}; {Name = "pc"; IntVal = 0}]
    let initModel = {Text = "a";  Registers = initReg}
    
    createApp initModel view update Virtualdom.createRender
    |> withStartNodeSelector "#test"
    |> start

    
    let editId = getById<Fable.Import.Browser.HTMLTextAreaElement> "code"
    printfn "Creating editor"
    let cmEditor = App.CodeMirrorImports.CodeMirror.fromTextArea(editId, initOptions)
    printfn "Setting editor value"
    cmEditor.setValue " abc def *** //comment"
    printfn "Line tokens: %A" (cmEditor.getLineTokens 0)
    printfn "Main Code finished"
    
main()

