module ErrorHandler

open Machine

exception CustomException of string

/// List of all possible errors, add all cases here
/// ADD YOUR ERRORS HERE
type IError =
    | ParseError of string
    | ALUError
    | MemoryError

/// Explanation of all errors, to display on GUI
/// AND EXPLAIN HERE
let errorHandler = function
    | ParseError e -> "parse error, " + e
    | ALUError -> "alu error"
    | MemoryError -> "memory error"

/// THIS IS THE TYPE THAT SHOULD BE RETURNED BY YOUR FUNCTIONS
/// Type that gets passed inside the parse -> execute pipeline
type Maybe<'T> =
    | Error of IError
    | Success of 'T

/// AND EVERYTHING DOWN HERE SHOULD NOT REALLY CONCERN YOU ;)
/// Define the maybe monad
type MaybeBuilder() =
    member this.Return(x) =  Success x
    member this.Bind (x,f) = 
        match x with
        | Success s -> f s      // a succeeds - use it
        | Error e -> Error e    // a fails - return error

/// can now be called in the following way
/// where parse and execute output Maybe<PaALUInstructionrse> and Maybe<State> resp. 
(*  maybe { 
    let! parsed = parse string
    let! newMachine = execute parsed
    return newMachine 
    }
*)
let maybe = MaybeBuilder()
