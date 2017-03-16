# How to use the Error Handler

1. include `module ErrorHandler` in your module
2. instead of returning your previous type like `AluInstruction` or `State` instead wrap it in the Maybe type, so instead of previousely
``` 
let parse str =
    ...
    instructions

let execute instructions =
    ...
    state
```
you do
```
let parse str =
    ...
    Success instructions

let execute instructions =
    ...
    Success state
```
that will do the trick to make it work again
3. Add Error handling. So if something can go wrong in your code 
```
match ... with
| OK a -> Success a
| NOTOK -> Error ERRORTYPE
```
now your function should still return the `Maybe<\'a>` type as previousely changed! You also have to add the `ERRORTYPE` to the `ErrorHandler` module and add an explanation. That ensures that the error can be explained in the GUI, so you would add 
```
type IError =
    | ParseError
    | ...
    | ERRORTYPE 

let errorHandler = function
    | ParseError -> "parse error, ..."
    | ...
    | ERRORTYPE -> "explain error here"
```
