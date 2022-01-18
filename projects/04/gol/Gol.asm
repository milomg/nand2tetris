// makes "acorn" in the center of the screen
@2
D = A
@20432
M = D     // M[20432] = 2
@8
D = A
@20464
M = D     // M[20464] = 8
@115
D = A
@20496
M = D     // M[20496] = 115

// initialize screen2 and R9
@8192
D = A
@screen2
M = D     // screen2 = 0x2000
@R9
M = 0     // R9 = 0


(MAIN_LOOP)
  // We first access the keyboard and store it in D
  @KBD
  D = M
  // We'll have the past value of KBD in R0, so if R0-D = 0, we just return to mainloop
  @R9
  D = D-M
  @MAINLOOP
  D; JEQ
  // If we get here, D!=R0, so re-grab KBD and update R0
  // if the program is slow, KBD might have changed since the last time we
  // accessed it, but doesn't break anything
  @KBD
  D = M
  @R9
  M = D

  // I'm arbitrarily picking keydown as the trigger for step, but you could use
  // keyup instead. That means that we should return to the mainloop if D = 0

  @MAIN_LOOP
  D; JEQ

  // now time to do the GOL algorithm!!
  // first, initialize the x and y iterators (R10, R11) to 0
  @R10
  M = 0
  @R11
  M = 0
  (STEP_LOOP)
    // we increment R10, and then if R10 is 512, we increment R11
    @R10
    M = M+1                   // R10++
    D = M
    @512
    D = D-A
    @STEP_LOOP_INC_CONTINUE
    D; JLT                    // if R10 < 512, goto STEP_LOOP_NO_INCREMENT
    @R10
    M = 0                     // R10 = 0
    @R11
    M = M+1                   // R11++
    D = M
    @256
    D = D-A
    @STEP_LOOP_INC_CONTINUE
    D; JLT                    // if R11 < 256, goto STEP_LOOP_NO_EXIT
    // if we get here, we've finished with the screen and should exit
    @STEP_LOOP_EXIT
    0; JMP
    (STEP_LOOP_INC_CONTINUE)

    // now we need to call GET_PIXEL and GET_NEIGHBORS

    @R10
    D = M
    @R1
    M = D       //R1 = R10
    @R11
    D = M
    @R2
    M = D       //R2 = R11
    @GET_NEIGHBORS_CONTINUE
    D = A
    @R0
    M = D       //R0 = (GET_NEIGHBORS_CONTINUE)
    @GET_NEIGHBORS
    0;JMP       // call GET_NEIGHBORS
    (GET_NEIGHBORS_CONTINUE)

    @R10
    D = M
    @R1
    M = D       //R1 = R10
    @R11
    D = M
    @R2
    M = D       //R2 = R11
    @GET_PIXEL_CONTINUE
    D = A
    @R0
    M = D       //R0 = (GET_PIXEL_CONTINUE)
    @GET_PIXEL
    0;JMP       // call GET_PIXEL
    (GET_PIXEL_CONTINUE)

    // #neighbors is in R7
    // value is in R3
    // Do a bunch of if checks and store the new value for the pixel in R3
    @R3
    D = M
    @MAIN_LOOP_IF_1
    D; JEQ              //if R3 = 0 goto MAIN_LOOP_IF_1
    @R7
    D = M
    @2
    D = D-A
    @MAIN_LOOP_IF_DEAD
    D; JLT              //if R7 < 2 goto MAIN_LOOP_IF_DEAD
    D = D+1
    @MAIN_LOOP_IF_DEAD
    D; JGT              //if R7 > 3 goto MAIN_LOOP_IF_DEAD
    @MAIN_LOOP_IF_ALIVE
    0; JMP              //else goto MAIN_LOOP_IF_ALIVE

    (MAIN_LOOP_IF_1)
    @R7
    D = M
    @3
    D = D-A
    @MAIN_LOOP_IF_ALIVE
    D; JEQ              //if R7 = 3 goto MAIN_LOOP_IF_ALIVE
    @MAIN_LOOP_IF_DEAD
    0; JMP              //else goto MAIN_LOOP_IF_DEAD

    (MAIN_LOOP_IF_DEAD)
    @R3
    M = 0
    @MAIN_LOOP_IF_EXIT
    0; JMP

    (MAIN_LOOP_IF_ALIVE)
    @R3
    M = 1
    @MAIN_LOOP_IF_EXIT
    0; JMP

    (MAIN_LOOP_IF_EXIT)
    // now we need to call SET_PIXEL (R3 is already set)
    @R10
    D = M
    @R1
    M = D       //R1 = R10
    @R11
    D = M
    @R2
    M = D       //R2 = R11
    @SET_PIXEL_CONTINUE
    D = A
    @R0
    M = D       //R0 = (SET_PIXEL_CONTINUE)
    @SET_PIXEL
    0;JMP       // call SET_PIXEL
    (SET_PIXEL_CONTINUE)

  @STEP_LOOP
  0;JMP
  (STEP_LOOP_EXIT)
  // finally, copy screen2 onto the screen
  // since we don't need R10 anymore, we'll reuse it as our iterator
  // @screen2
  // D = A
  // @R10
  // M = D         // R10 = &(screen2)
  // (COPY_LOOP)
  //   @R10
  //   D = M
  //   @SCREEN
  //   D = D-A
  //   @COPY_LOOP_EXIT
  //   D; JGE      // if R10 >= &(SCREEN) goto COPY_LOOP_EXIT
  //
  //   @R10
  //   D = M
  //   @8192
  //   D = D+A
  //   @R11
  //   M = D       // R11 = R10+8192
  //   @R10
  //   A = M
  //   D = M       // D = M[R10]
  //   @R11
  //   A = M
  //   M = D       //M[R10+8192] = M[R10]
  //
  //   @R10
  //   M = M+1     //R10++
  // @COPY_LOOP
  // 0;JMP
  // (COPY_LOOP_EXIT)

@MAIN_LOOP
0;JMP

(GET_PIXEL)
  // since we might request a pixel outside of the screen, we'll assume they're
  // dead if they're OOB
  @R1
  D = M
  @GET_PIXEL_OOB
  D; JLT          // if R1<0, goto GET_PIXEL_OOB
  @R1
  D = M
  @64
  D = D-A
  @GET_PIXEL_OOB
  D; JGE          // if R1>=512, goto GET_PIXEL_OOB
  @R2
  D = M
  @GET_PIXEL_OOB
  D; JLT          // if R1<0, goto GET_PIXEL_OOB
  @R2
  D = M
  @32
  D = D-A
  @GET_PIXEL_OOB
  D; JGE          // if R1>=256, goto GET_PIXEL_OOB

  @GET_PIXEL_OOB_SKIP
  0; JMP

  (GET_PIXEL_OOB)
  // set R3 to 0 and jump back to M[R0]
  @R3
  M = 0;
  @R0
  A = M
  0; JMP

  (GET_PIXEL_OOB_SKIP)
  //multiply x and y value by 8

  // OLD CODE
  // first we find the 16-bit chunk that the position corresponds to:
  // SCREEN + y*32 + x/16
  // we'll store this in R2 since we won't need R2 after this
  @R2
  D = M
  M = D+M
  D = M
  M = D+M
  D = M
  M = D+M
  D = M
  M = D+M
  D = M
  M = D+M   // R2 = y*32
  // now for division by 16!!! The way we'll do this is by repeatedly subtracting
  // 16 from R1 and keeping track in R2. That we we'll also end up with x%16 in R1
  (GET_PIXEL_LOOP_1)
    @R1
    D = M
    @16
    D = D-A
    @GET_PIXEL_LOOP_1_EXIT
    D; JLT    // if R1<16 goto GET_PIXEL_LOOP_1_EXIT

    @16
    D = A
    @R1
    M = M-D // R1-=16

    @R2
    M = M + 1 // R2++

    @GET_PIXEL_LOOP_1
    0; JMP
  (GET_PIXEL_LOOP_1_EXIT)

  // after the loop:
  // R1 = x%16
  // R2 = y*32 + x/16
  @SCREEN
  D = A
  @R2
  M = M+D   // R2 = SCREEN + y*32 + x/16
  // Now I want to get 2^R1 somewhere
  // I can just put the current value of R1 in R4 and then have R1 be 2^R4
  @R1
  D = M
  M = 1
  @R4
  M = D
  (GET_PIXEL_LOOP_2)
    @R4
    D = M
    @GET_PIXEL_LOOP_2_EXIT
    D; JLE

    @R4
    M = M - 1
    @R1
    D = M
    M = M + D
    @GET_PIXEL_LOOP_2
    0;JMP
  (GET_PIXEL_LOOP_2_EXIT)
  //after the loop R1 = 2^(x%16)

  // find the content of the word at R2
  @R2
  A = M
  D = M
  // and it with R1 to access the value of the pixel
  @R1
  D = M & D
  // if D is 0, the pixel is 0, otherwise the pixel is 1
  @GET_PIXEL_IF_1
  D;JEQ
    @R3
    M = 1
  @GET_PIXEL_IF_2
  0;JMP
  (GET_PIXEL_IF_1)
    @R3
    M = 0
  (GET_PIXEL_IF_2)
  // R3 is now correct, so we can return to wherever R0 is.
@R0
A = M
0;JMP

(GET_NEIGHBORS)
  // to aviod conflict with GET_PIXEL:
  // we'll store the answer in R7
  // the return location in R8
  // R5 = x, R6 = y
  @R1
  D = M
  @R5
  M = D     // R5 = R1
  @R2
  D = M
  @R6
  M = D     // R6 = R2
  @R0
  D = M
  @R8
  M = D     // R8 = R0
  @R7
  M = 0     // R7 = 0

  @R5
  D = M
  @R1
  M = D-1   // R1 = R5-1
  @R6
  D = M
  @R2
  M = D     // R2 = R6
  @GET_NEIGHBORS_RETURN_1
  D = A
  @R0
  M = D     // R0 = (GET_NEIGHBORS_RETURN_1)
  @GET_PIXEL
  0;JMP     // call GET_PIXEL((GET_NEIGHBORS_RETURN_1), R5-1, R6)
  (GET_NEIGHBORS_RETURN_1)
  // update R7
  @R3
  D = M
  @R7
  M = M+D   //R7 += R3

  @R5
  D = M
  @R1
  M = D-1   // R1 = R5-1
  @R6
  D = M
  @R2
  M = D+1     // R2 = R6+1
  @GET_NEIGHBORS_RETURN_2
  D = A
  @R0
  M = D     // R0 = (GET_NEIGHBORS_RETURN_2)
  @GET_PIXEL
  0;JMP     // call GET_PIXEL((GET_NEIGHBORS_RETURN_2), R5-1, R6+)
  (GET_NEIGHBORS_RETURN_2)
  // update R7
  @R3
  D = M
  @R7
  M = M+D   //R7 += R3

  @R5
  D = M
  @R1
  M = D     // R1 = R5
  @R6
  D = M
  @R2
  M = D+1   // R2 = R6+1
  @GET_NEIGHBORS_RETURN_3
  D = A
  @R0
  M = D     // R0 = (GET_NEIGHBORS_RETURN_3)
  @GET_PIXEL
  0;JMP     // call GET_PIXEL((GET_NEIGHBORS_RETURN_3), R5, R6+1)
  (GET_NEIGHBORS_RETURN_3)
  // update R7
  @R3
  D = M
  @R7
  M = M+D   //R7 += R3

  @R5
  D = M
  @R1
  M = D+1   // R1 = R5+1
  @R6
  D = M
  @R2
  M = D+1   // R2 = R6+1
  @GET_NEIGHBORS_RETURN_4
  D = A
  @R0
  M = D     // R0 = (GET_NEIGHBORS_RETURN_4)
  @GET_PIXEL
  0;JMP     // call GET_PIXEL((GET_NEIGHBORS_RETURN_4), R5+1, R6+1)
  (GET_NEIGHBORS_RETURN_4)
  // update R7
  @R3
  D = M
  @R7
  M = M+D   //R7 += R3

  @R5
  D = M
  @R1
  M = D+1   // R1 = R5+1
  @R6
  D = M
  @R2
  M = D   // R2 = R6
  @GET_NEIGHBORS_RETURN_5
  D = A
  @R0
  M = D     // R0 = (GET_NEIGHBORS_RETURN_5)
  @GET_PIXEL
  0;JMP     // call GET_PIXEL((GET_NEIGHBORS_RETURN_5), R5+1, R6)
  (GET_NEIGHBORS_RETURN_5)
  // update R7
  @R3
  D = M
  @R7
  M = M+D   //R7 += R3

  @R5
  D = M
  @R1
  M = D+1   // R1 = R5+1
  @R6
  D = M
  @R2
  M = D-1   // R2 = R6-1
  @GET_NEIGHBORS_RETURN_6
  D = A
  @R0
  M = D     // R0 = (GET_NEIGHBORS_RETURN_6)
  @GET_PIXEL
  0;JMP     // call GET_PIXEL((GET_NEIGHBORS_RETURN_6), R5+1, R6-1)
  (GET_NEIGHBORS_RETURN_6)
  // update R7
  @R3
  D = M
  @R7
  M = M+D   //R7 += R3

  @R5
  D = M
  @R1
  M = D     // R1 = R5
  @R6
  D = M
  @R2
  M = D-1   // R2 = R6-1
  @GET_NEIGHBORS_RETURN_7
  D = A
  @R0
  M = D     // R0 = (GET_NEIGHBORS_RETURN_7)
  @GET_PIXEL
  0;JMP     // call GET_PIXEL((GET_NEIGHBORS_RETURN_7), R5, R6-1)
  (GET_NEIGHBORS_RETURN_7)
  // update R7
  @R3
  D = M
  @R7
  M = M+D   //R7 += R3

  @R5
  D = M
  @R1
  M = D-1   // R1 = R5-1
  @R6
  D = M
  @R2
  M = D-1   // R2 = R6-1
  @GET_NEIGHBORS_RETURN_8
  D = A
  @R0
  M = D     // R0 = (GET_NEIGHBORS_RETURN_8)
  @GET_PIXEL
  0;JMP     // call GET_PIXEL((GET_NEIGHBORS_RETURN_8), R5-1, R6-1)
  (GET_NEIGHBORS_RETURN_8)
  // update R7
  @R3
  D = M
  @R7
  M = M+D   //R7 += R3

@R8 // don't forget we moved the return location into here instead of R0
A = M
0;JMP

(SET_PIXEL)
  // this is pretty similar to GET_PIXEL, so I copied a bunch of code
  // first we find the 16-bit chunk that the position corresponds to:
  // SCREEN + y*32 + x/16
  // we'll store this in R2 since we won't need R2 after this
  @R2
  D = M
  M = D+M
  D = M
  M = D+M
  D = M
  M = D+M
  D = M
  M = D+M
  D = M
  M = D+M   // R2 = y*32
  // now for division by 16!!! The way we'll do this is by repeatedly subtracting
  // 16 from R1 and keeping track in R2. That we we'll also end up with x%16 in R1
  (SET_PIXEL_LOOP_1)
    @R1
    D = M
    @16
    D = D-A
    @SET_PIXEL_LOOP_1_EXIT
    D; JLT    // if R1<16 goto SET_PIXEL_LOOP_1_EXIT

    @16
    D = A
    @R1
    M = M - D // R1-=16

    @R2
    M = M + 1 // R2++

    @SET_PIXEL_LOOP_1
    0; JMP
  (SET_PIXEL_LOOP_1_EXIT)

  // after the loop:
  // R1 = x%16
  // R2 = y*32 + x/16
  // IMPORTANT!!! THIS IS NOT SCREEN
  // instead I'm writing to a fake screen which will be copied over afterwards
  @SCREEN
  D = A
  @R2
  M = M+D   // R2 = SCREEN + y*32 + x/16
  // Now I want to get 2^R1 somewhere
  // I can just put the current value of R1 in R4 and then have R1 be 2^R4
  @R1
  D = M
  M = 1
  @R4
  M = D
  (SET_PIXEL_LOOP_2)
    @R4
    D = M
    @SET_PIXEL_LOOP_2_EXIT
    D; JLE

    @R4
    M = M - 1
    @R1
    D = M
    M = M + D
    @SET_PIXEL_LOOP_2
    0;JMP
  (SET_PIXEL_LOOP_2_EXIT)
  //after the loop R1 = 2^(x%16)

  // If we want to set the pixel to 1, we or R1 and M[R2]
  // If we want to set the pixel to 0, we and !R1 and M[R2]
  @R3
  D = M
  @SET_PIXEL_IF_1
  D;JEQ
    // R3 = 1
    @R1
    D = M
    @R2
    A = M
    M = M | D // M[R2] = R1 | M[R2]
  @SET_PIXEL_IF_2
  0;JMP
  (SET_PIXEL_IF_1)
    // R3 = 1
    @R1
    D = !M
    @R2
    A = M
    M = M & D // M[R2] = !R1 & M[R2]
  (SET_PIXEL_IF_2)
@R0
A = M
0;JMP
