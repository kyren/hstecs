@R2 // Stores R0*R1 to R2
M=0
@i // Loop Counter
M=0

// Add R0 to R2 R1 times (R2=R0*R1)
(MULTLOOP)
  @i
  D=M
  @R1
  D=D-M
  @END
  D;JGE // Jump if i >= R1

  @R0
  D=M
  @R2
  M=D+M // Add R0 to R2

  @i
  M=M+1 // i += 1
  @MULTLOOP
  0;JMP // Loop while i < R1
(END)

// R2 now has correct value, done.
@END
0;JMP
