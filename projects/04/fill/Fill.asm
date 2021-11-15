// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(CHECK)        // Check if a key is pressed and jump to either fill or clear.
    @KBD
    D=M

    @CLEAR
    D;JEQ

    @FILL
    0;JMP

(FILL)          // Set the fill color to black and see if we should start the fill loop
    @fill_color // Set the fill color to black.
    MD=-1       // Also store the color to check with the screen color in LOOP_START

    @LOOP_START // Jump to the fill loop
    0;JMP

(CLEAR)
    @fill_color // Set the fill color to white
    MD=0        // Also store the color to check with the screen color in LOOP_START

(LOOP_START)
    @SCREEN     // Check the screen color
    D=M-D       // If the first screen color is the same as the fill color,
    @CHECK
    D;JEQ       // bail on the LOOP and jump back to CHECK.

    @SCREEN     // Store the start of the screen in i 
    D=A
    @i
    M=D

(LOOP)
    @fill_color
    D=M         // Store the fill color in D

    @i
    A=M
    M=D        // Write the fill color (D) to RAM[i]

    @i
    MD=M+1     // Increment i, and store in D so we can check if we're done.
    
    @KBD
    D=D-A      // If i==KBD, we have finished the loop (because the screen ends exactly at KBD-1), 

    @CHECK     // we jump to CHECK
    D;JEQ

    @LOOP      // Otherwise, we continue the LOOP
    D;JNE
