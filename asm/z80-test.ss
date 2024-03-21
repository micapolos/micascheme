(import (check) (micascheme) (asm core) (asm z80))

(define-syntax-rule (check-assembles? $asm $bytevector)
  (check (equal? (asm-bytevector $asm) $bytevector)))

(check-assembles?
  (asm (db))
  (bytevector))

(check-assembles?
  (asm (db #x12))
  (bytevector #x12))

(check-assembles?
  (asm (db #\f))
  (bytevector (char->integer #\f)))

(check-assembles?
  (asm (db (integer->char 100)))
  (bytevector 100))

(check-assembles?
  (asm (db "fo"))
  (bytevector (char->integer #\f) (char->integer #\o)))

(check-assembles?
  (asm (db #x12 #x34))
  (bytevector #x12 #x34))

(check-assembles?
  (asm (dw))
  (bytevector))

(check-assembles?
  (asm (dw #x1234))
  (bytevector #x34 #x12))

(check-assembles?
  (asm (dw #x1234 #x5678))
  (bytevector #x34 #x12 #x78 #x56))

(check-assembles?
  (asm (org #x1234))
  (bytevector))

(check-assembles?
  (asm (dz #x12))
  (bytevector #x12 0))

(check-assembles?
  (asm (ds 3))
  (bytevector 0 0 0))

(check-assembles?
  (asm
    (align 4)
    (db #xff))
  (bytevector #xff))

(check-assembles?
  (asm
    (db #x12)
    (align 4)
    (db #xff))
  (bytevector #x12 0 0 0 #xff))

(check-assembles?
  (asm
    (db #x12 #x34)
    (align 4)
    (db #xff))
  (bytevector #x12 #x34 0 0 #xff))

(check-assembles?
  (asm
    (db #x12 #x34 #x56)
    (align 4)
    (db #xff))
  (bytevector #x12 #x34 #x56 0 #xff))

(check-assembles?
  (asm
    (db #x12 #x34 #x56 #x78)
    (align 4)
    (db #xff))
  (bytevector #x12 #x34 #x56 #x78 #xff))

