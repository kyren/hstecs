// R1 - Screen Begin
@16384
D=A
@R1
M=D

// R2 - Screen End (inclusive)
@24575
D=A
@R2
M=D

// R3 - Keyboard
@24576
D=A
@R3
M=D

@R1
D=M
@R0
M=D

(DRAWLOOP)
  // First, reset if pointer is past screen end.
  @R2
  D=M
  @R0
  D=D-M
  @DRAWRESET
  D;JLT
  // If value at R2 pointer (keyboard) is non-zero, draw black, otherwise draw white.
  @R3
  A=M
  D=M
  @DRAWBLACK
  D;JNE
  @DRAWWHITE
  0;JMP

  // Set value at R0 pointer to -1 (all on);
  (DRAWBLACK)
  @R0
  A=M
  M=-1
  @DRAWCONTINUE
  0;JMP

  // Set value at R0 pointer to 0 (all off);
  (DRAWWHITE)
  @R0
  A=M
  M=0
  @DRAWCONTINUE
  0;JMP

  // Increment R0 pointer to next 16 bit pixel block
  (DRAWCONTINUE)
  @R0
  M=M+1
  @DRAWLOOP
  0;JMP

  // Reset R0 pointer back to R1 (beginning of screen memory)
  (DRAWRESET)
  @R1
  D=M
  @R0
  M=D
  @DRAWLOOP
  0;JMP
