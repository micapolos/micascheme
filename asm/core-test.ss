(import (check) (asm core) (micascheme))

(define-syntax-rule (check-assembles? $asm $bytevector)
  (check (equal? (asm-bytevector $asm) $bytevector)))
