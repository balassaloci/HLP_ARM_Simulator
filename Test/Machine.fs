module Machine

type RegisterIndex = 
    | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
    | R9 | R10 |R11 | R12 | R13 | LR | PC | CSPR


type State = 
    private
        { Register : Map<RegisterIndex,int>
          Memory : Map<uint32,int> }

[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module State = 

    let registerValue reg state = Map.find reg state.Register

    let memoryValue mem state = Map.find mem state.Memory

    let updateRegister reg value state =
        {Register=Map.add reg value state.Register; Memory=state.Memory}

    let updateMemory mem value state = Map.add mem value state.Memory

let initialMemory = Map.empty<uint32,int>

let regList = 
    [(R0,0); (R1,0); (R2,0); (R3,0); (R4,0); (R5,0); (R6,0); (R7,0); 
    (R8,0); (R9,0); (R10,0); (R12,0); (R13,0); (LR,0); (PC,0); (CSPR,0)]
    
let initialRegisters = Map.ofList regList


let mutable state = {Register = initialRegisters;Memory = initialMemory}


