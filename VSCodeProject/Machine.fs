module Machine
open ErrorHandler

type RegisterIndex = 
    | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
    | R9 | R10 |R11 | R12 | R13 | LR | PC

type StatusBit = | N | Z | C | V

type State<'a> = 
    private
        { Register : Map<RegisterIndex,int>
          Memory : Map<int,int>
          Address : int
          Status : Map<StatusBit,bool>
          Instructions : array<'a>
          Labels : Map<string,int>
          End: bool}

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
    let getWordFromMemory address state = 
        if address%4 = 0 then 
            if Map.containsKey (address/4) state.Memory then
                Map.find (address/4) state.Memory
            else 0
        else
            failc "Memory address in not divisible by 4"

    ///Takes a memory address and state as input
    ///Returns the byte value stored at the address        
    let getByteFromMemory address state =
        let byteIndex = address % 4
        let shift = byteIndex * 8
        let memoryWord = Map.find (address/4) state.Memory
        let temp = uint32((255 <<< shift) &&& memoryWord) 
        int (temp >>> shift)
        

    /// Take a memory address, a value and state as input
    /// Returns a new state with the updated address
    let updateWordInMemory address value state = 
        if address % 4 = 0 then
            {state with Memory = Map.add (address/4) value state.Memory} 
        else
            failc "Memory address is not divisible by 4"
    
    ///Takes a memory address, a value and state as input
    ///Returns a new state with the updated byte
    let updateByteInMemory address value state =
        let byteIndex = address % 4
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
    
    ///Takes a memory address and state as input
    ///Returns a new state with the memory address removed
    let deleteWordInMemory address state =
        {state with Memory = Map.remove (address/4) state.Memory}
    
    ///Takes the state as input
    ///Returns the lowest available memory address 
    ///Used for DCD,FILL that do not specify address 
    let getMemoryAddress state = state.Address

    ///Updates the lowest available memory address
    let updateMemoryAddress adr state =
        {state with Address = adr} 

    /// Use State.registerValue
    /// Function used by old code
    /// Takes a register and state as input
    /// Returns the value in that register
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

    ///Takes state as input
    ///Returns a map of memory
    let getMemory state = state.Memory

    /// Takes state as input
    /// Returns a map of status bits
    let getStatus state = state.Status

    ///Takes an instructions address and state as input
    ///Returns the instruction corresponding to that address
    let getInstruction (address:int) (state:State<'a>) =
        let index = (address / 4) - 1
        if index < Array.length state.Instructions then
            Some (Array.get state.Instructions index)
        else None

    ///Takes an instruciton address and state as input
    ///Returns true if instruction exists
    let instructionExists (address:int) state =
        let index = (address / 4) - 1
        index < Array.length state.Instructions

    ///Takes the state as input
    ///Returns the address of the last instruction
    let lastInstructionAddress state = 
        let length = Array.length state.Instructions
        (length - 1) * 4

    ///Takes a label, address and state as input
    ///Returns a new state with the label stored
    let addLabelAddress label address state =
        if Map.containsKey label state.Labels then
            failc "Connot overwrite labels"
        else
            {state with Labels = Map.add label address state.Labels}

    ///Takes a label and state as input
    ///Returns the address corresponding to that label
    let getLabelAddress label state = 
        if Map.containsKey label state.Labels then
            Map.find label state.Labels
        else failc "Label does not exist"

    ///Takes the state as input
    ///Increments the Program Counter by 4
    let incrementPC state =
        let oldPC = systemRegisterValue PC state
        updateRegister PC (oldPC + 4) state

    ///Sets the Machine state end boolean to true
    let endExecution state = 
        {state with End = true}
    
    ///Checks if the execution has ended
    let checkEndExecution state = state.End

    /// Takes the array of instructins and a map of labels as input
    /// Returns the initial state    
    let makeInitialState (instructions:array<'a>) labels = 
        let regList = 
            [(R0,0); (R1,0); (R2,0); (R3,0); (R4,0); (R5,0); (R6,0); (R7,0); 
            (R8,0); (R9,0); (R10,0); (R11,0); (R12,0); (R13,0xFF000000); (LR,0);(PC,4)]
        let initialRegisters = Map.ofList regList
        let initialStatus =
            [(N, false); (Z, false); (C, false); (V, false)]
        let iLength = Array.length instructions
        let address = ((iLength / 256) * 256) + 256 
        {Register = initialRegisters; 
        Memory = Map.empty<int,int>;
        Address = address;
        Status = Map.ofList initialStatus;
        Instructions = instructions;
        Labels = labels;
        End = false} 
