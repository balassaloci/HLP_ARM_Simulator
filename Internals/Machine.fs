module Machine

type Register = | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 |R11 | R12 | R13 | LR | PC 

// Initialize registers to 0
let registers = Map.ofList [
                            R0,0; R1,0; R2,0; R3,0; 
                            R4,0; R5,0; R6,0; R7,0; 
                            R8,0; R9,0; R10,0; R11,0; 
                            R12,0; R13,0; LR,0; PC,0
                            ]
