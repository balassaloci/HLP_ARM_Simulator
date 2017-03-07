module Machine

type RegisterIndex = 
    | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
    | R9 | R10 |R11 | R12 | R13 | LR | PC 

type CSPR = {C : bool; S : bool; P : bool; R : bool}

type State = 
    private
        { Register : Map<RegisterIndex,int>
          Memory : Map<int,int> 
          Status : CSPR}

[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module State = 

    /// Takes a register and state as input
    /// Returns the value of the register
    let registerValue reg state = Map.find reg state.Register

    ///Takes a memory address and state as input
    ///Returns the value stored at the address
    let memoryValue mem state = Map.find mem state.Memory

    /// Takes a register, a value and state as input
    /// Returns a new state with the updated register
    let updateRegister reg value state = 
        {Register=Map.add reg value state.Register; Memory=state.Memory; Status = state.Status}

    /// Take a memory address, a value and state as input
    /// Returns a new state with the updates address
    let updateMemory mem value state = 
        {Register = state.Register; Memory = Map.add mem value state.Memory; Status = state.Status}

    /// Returns a boolean for status bit C 
    let getN state = state.Status.C

    /// Returns a boolean for status bit S
    let getZ state = state.Status.S

    /// Returns a boolean for status bit C
    let getC state = state.Status.P

    /// Returns a boolean for status bit C
    let getV state = state.Status.R

    /// Takes a boolean and state as input
    /// Returns a new state with updated status bit C
    let setN v state = 
        let newStatus = {C = v; S = state.Status.S; P = state.Status.P; R = state.Status.R}
        {Register = state.Register; Memory = state.Memory; Status = newStatus}
    
    /// Takes a boolean and state as input
    /// Returns a new state with updated status bit S
    let setZ v state = 
        let newStatus = {C = state.Status.C; S = v; P = state.Status.P; R = state.Status.R}
        {Register = state.Register; Memory = state.Memory; Status = newStatus}
    
    /// Takes a boolean and state as input
    /// Returns a new state with updated status bit P
    let setC v state = 
        let newStatus = {C = state.Status.C; S = state.Status.S; P = v; R = state.Status.R}
        {Register = state.Register; Memory = state.Memory; Status = newStatus}
    
    /// Takes a boolean and state as input
    /// Returns a new state with updated status bit R
    let setV v state = 
        let newStatus = {C = state.Status.C; S = state.Status.S; P = state.Status.P; R = v}
        {Register = state.Register; Memory = state.Memory; Status = newStatus}
   
    /// Initializes state
    /// Registers and Memory set to 0
    /// Status registers set to false    
    let makeInitialState () = 
        let initialMemory = Map.empty<int,int>
        let regList = 
            [(R0,0); (R1,5); (R2,0); (R3,0); (R4,0); (R5,0); (R6,0); (R7,0); 
            (R8,0); (R9,0); (R10,0); (R12,0); (R13,0); (LR,0); (PC,0); (CSPR,0)]
        let initialRegisters = Map.ofList regList
        let initialStatus = {C = false; S = false; P = false; R = false}

        {Register = initialRegisters; Memory = initialMemory; Status = initialStatus}
