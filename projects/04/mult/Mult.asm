// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
//
// This program only needs to handle arguments that satisfy
// R0 >= 0, R1 >= 0, and R0*R1 < 32768.


// Move the variables to temporary registers
    @R1
    D=M
    @R4
    M=D
    @R0
    D=M
    @R3
    M=D

// Swap the variables to make things faster
// ----------------------------------------------------------------------------
    @R3
    D=M
    @R4
    D=D-M

    @AFTER_SWAP
    D;JLT        // if R0 < R1, jump to AFTER_SWAP because the numbers are in the correct order

    @R4          // else, swap R0 and R1 so that the smaller number is in R0
    M=M+D        // We use Alfred's swap here, R1=R1+(R0-R1)=R0
    @R3
    M=M-D        // Alfred's swap pt2 here, R0=R0-(R0-R1)=R1

(AFTER_SWAP)
// ----------------------------------------------------------------------------
// Everything betwee the above two lines is just an optimization to make fewer adds

    @R2         // Make sure R2 is 0
    M=0

    @R3         // Store the value of R0 in D for use in the loop
    D=M

(LOOP)
    @END    // We start a while (R0 != 0) loop
    D;JEQ
    @R4     
    D=M     // Read the value of R1
    @R2
    M=M+D   // Add R1 to out (R2)
    @R3
    MD=M-1  // Decrement R0
    @LOOP
    0;JMP   // Jump back to LOOP
(END)       // We loop forever at the end of the program
    @END
    0;JMP