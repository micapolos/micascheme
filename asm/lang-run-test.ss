(import (asm lang))

(define-fragment empty)
(define-fragment dw-empty (dw empty))

(check-asm
  (org #xc000)
  (dw empty)
  (dw dw-empty)
  (asm
    (start 49153)
    (db 201 0 192)))
