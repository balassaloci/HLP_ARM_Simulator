module Machine

type RegisterIndex = 
    | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
    | R9 | R10 |R11 | R12 | R13 

//type CSPR = {C : bool; S : bool; P : bool; R : bool}

type StatusBit = | N | Z | C | V
type SystemRegister = |LR | PC 
type System = |Registers of SystemRegister | Bits of StatusBit

type SystemValue = |RegisterValue of int | BitValue of bool

type State = 
    private
        { Register : Map<RegisterIndex,int>
          Memory : Map<int,int> 
          Status : Map<System,SystemValue>}

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
    let updateRegister reg value state = {state with Register = Map.add reg value state.Register}

    /// Take a memory address, a value and state as input
    /// Returns a new state with the updates address
    let updateMemory mem value state = {state with Memory = Map.add mem value state.Memory}

    /// Returns value of register LR
    let systemRegisterValue (reg:SystemRegister) state = Map.find (Registers reg) state.Status

    /// Returns value of the Program Counter
    let statusBitValue (bit:StatusBit) state = Map.find (Bits bit) state.Status
    
    /// Takes a boolean and state as input
    /// Returns a new state with updated status bit C

    /// Takes a system register, value and state as input
    /// Returns a new state with updated system register
    let updateSystemRegister (reg:SystemRegister) (value:int) state = 
        {state with Status = Map.add (Registers reg) (RegisterValue value) state.Status}

    /// Takes a status bit, value and state as input
    /// Returns a new state with updated status bit
    let updateStatusBit (bit:StatusBit) (b:bool) state = 
        {state with Status = Map.add (Bits bit) (BitValue b) state.Status}

    /// Takes state as input
    /// Returns map of registers
    let getRegisters state = state.Register

    /// Takes state as input
    /// Returns a map of system registers and status bits
    let getStatus state = state.Status
   
    /// Initializes state
    /// Registers and Memory set to 0
    /// Status registers set to false    
    let makeInitialState () = 
        let initialMemory = Map.empty<int,int>
        let regList = 
            [(R0,0); (R1,5); (R2,0); (R3,0); (R4,0); (R5,0); (R6,0); (R7,0); 
            (R8,0); (R9,0); (R10,0); (R12,0); (R13,0)]
        let initialRegisters = Map.ofList regList
        let initialStatus =
            [(Registers LR , RegisterValue 0); (Registers PC , RegisterValue 0); (Bits N , BitValue false); 
            (Bits Z , BitValue false); (Bits C, BitValue false); (Bits V, BitValue false)]

        {Register = initialRegisters; Memory = initialMemory; Status = Map.ofList initialStatus}
