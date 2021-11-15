
(CHECK)             // Check if a key is pressed and jump to either set odd to -1 or 0
    @KBD
    D=M

    @RIGHT_CHECK
    D;JEQ

(LEFT_CHECK)
    @odd
    M=-1            // Start with an even row

    @FILL_SCREEN
    0;JMP

(RIGHT_CHECK)
    @odd            // Start with an odd row
    M=0

(FILL_SCREEN)
    @SCREEN         // Store the start of the screen in i 
    D=A
    @i
    M=D

    @32             // Counter for how many rows we've drawn (we want to draw 32*8=256 pixels tall)
    D=A
    @rows
    M=D

(LOOP)
    @255
    D=A            // Store 0b0000000011111111 in fill_color
    @fill_color
    M=D

    @odd            // Check whether we are in an odd row or an even row and set the fill color
    D=M             // Then, we run the loop for one row of checkers

    @EVENROW
    D;JNE

    @END.ROWCOLOR
    D;JMP
(EVENROW)
    @fill_color
    M=!M            // Store 0b0000000011111111 in fill_color

(END.ROWCOLOR)      // I called this END at first which created an annoying bug
    @256
    D=A
    @counter        // We count down from 256 to draw 16*256=8*8*64 pixels for one vertical row
    M=D

    @odd            // Toggle wether or not we are in an odd row
    M=!M

    @rows           // Make sure we haven't drawn more than 32 rows
    MD=M-1
    @CHECK
    D+1;JEQ         // We check is the last number we drew was 0, then we are done with the 32 rows

(FILL_HORIZONTAL)
    @fill_color
    D=M

    @i
    M=M+1
    A=M-1
    M=D

    @counter
    MD=M-1

    @LOOP             // We check if we are at the end of the row
    D;JEQ             // Then, we jump back to LOOP to draw another row

    @FILL_HORIZONTAL  // Otherwise, we continue the row
    0;JMP
