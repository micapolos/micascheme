(import (asm-3 lang))

(const val-10 10)

(data foo
  (u8 10)
  (u8 val-10)
  (u8 (+ 10 val-10)))

(proc main
  (u8 10)
  (align 2)
  (u8 20)
  (begin
    (u16-le #x1234)
    (u16-be org)))

(check-assembled (org #xc000) foo
  (assembled
    (start #xc000)
    (db 10 10 20)))

(check-assembled (org #xc000) main
  (assembled
    (start #xc000)
    (db 10 0 20 #x34 #x12 #xc0 #x05)))
