(import (micascheme) (zx-next emu z80))

(define z80 (make-z80))

(check (equal? (z80-af z80) #x0000))
(set-z80-a! z80 #x12)
(set-z80-f! z80 #x34)
(check (equal? (z80-af z80) #x1234))
