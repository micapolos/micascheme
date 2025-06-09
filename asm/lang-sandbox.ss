(import (asm lang))

(db
  #xf3 ; DI
  #x3e ; LD A, 0
  #b00000010
  #xd3 ; OUT ($fe), A
  #xfe
  #x3e ; LD A, $ff
  #b00010101
  #xd3 ; OUT ($fe), A
  #xfe
  #xc3 ; JMP $c001
  #x01
  #xc0)

(start)
