module Machine

type RegisterIndex = 
    | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
    | R9 | R10 |R11 | R12 | R13 | LR | PC

type StatusBit = | N | Z | C | V


type State = 
    private
        { Register : Map<RegisterIndex,int>
          Memory : Map<int,int> 
          Status : Map<StatusBit,bool>
          Instructions : array<int>
          Labels : Map<string,int>} // change type to that of Instructions 

[<RequireQualifiedAccess; 
CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module State = 

    /// Takes a register and state as input
    /// Returns the value of the register
    let registerValue reg state = Map.find reg state.Register

    /// Takes a register, a value and state as input
    /// Returns a new state with the updated register
    let updateRegister reg value state = 
        {state with Register = Map.add reg value state.Register}

    ///Takes a memory address and state as input
    ///Returns the value stored at the address
    let getWordFromMemory address state = Map.find (address/4) state.Memory //error handling if address is not divisible by 4
    let getByteFromMemory address state =
        let byteIndex = 3 - (address % 4)
        let shift = byteIndex * 8
        let memoryWord = Map.find (address/4) state.Memory
        ((255 <<< shift) &&& memoryWord) >>> shift
        

    /// Take a memory address, a value and state as input
    /// Returns a new state with the updated address
    let updateWordInMemory address value state = 
        {state with Memory = Map.add (address/4) value state.Memory} //error if not divisible by 4

    let updateByteInMemory address value state =
        let byteIndex = 3 - (address % 4)
        let shift = byteIndex * 8 
        match Map.containsKey (address/4) state.Memory with
        | false ->
            {state with 
                Memory = Map.add (address/4) (value <<< shift) state.Memory}
        | true ->
            let oldWord = Map.find (address/4) state.Memory
            let newWord = (~~~(255 <<< shift) &&& oldWord) ||| ((255 <<< shift) &&& (value <<< shift)) //find a better method
            {state with 
                Memory = Map.add (address/4) newWord state.Memory}

    /// Takes a system register and state as input
    /// Returns the value in that system register
    let systemRegisterValue (reg:RegisterIndex) state = 
        registerValue reg state
        
    /// Takes a status bit and state as input
    /// Returns a boolean for that status bit
    let statusBitValue (bit:StatusBit) state = 
        Map.find bit state.Status
        
    /// Takes a system register, value and state as input
    /// Returns a new state with updated system register
    let updateSystemRegister (reg:RegisterIndex) (value:int) state = 
        updateRegister reg value state

    /// Takes a status bit, a boolean and state as input
    /// Returns a new state with updated status bit
    let updateStatusBit (bit:StatusBit) (b:bool) state = 
        {state with Status = Map.add bit b state.Status}

    /// Takes state as input
    /// Returns a map of registers
    let getRegisters state = state.Register

    let getMemory state = state.Memory

    /// Takes state as input
    /// Returns a map of system registers and status bits
    let getStatus state = state.Status

    let addInstruction instruction state =
        {state with Instructions = Array.append state.Instructions [|instruction|]} 
    
    let getInstruction (address:int) (state:State) =
        //let index = address / 4
        Array.get state.Instructions (address / 4)

    let lastInstructionAddress state = 
        let length = Array.length state.Instructions
        (length - 1) * 4

    let addLabelAddress label address state =
        {state with Labels = Map.add label address state.Labels}

    let getLabelAddress label state = Map.find label state.Labels

    let incrementPC state =
        let oldPC = systemRegisterValue PC state
        updateSystemRegister PC (oldPC + 4) state

    /// Initializes state
    /// Registers and Memory set to 0
    /// Status registers set to false    
    let makeInitialState () = 
        let regList = 
            [(R0,0); (R1,5); (R2,0); (R3,0); (R4,0); (R5,0); (R6,0); (R7,0); 
            (R8,0); (R9,0); (R10,0); (R12,0); (R13,0)]
        let initialRegisters = Map.ofList regList
        let initialStatus =
            [(N, false); (Z, false); (C, false); (V, false)]
        {Register = initialRegisters; 
        Memory = Map.empty<int,int>; 
        Status = Map.ofList initialStatus;
        Instructions = Array.empty<int>;
        Labels = Map.empty<string,int>} //change type to that of instructions
