(import (check) (asm core) (micascheme))

(define-rule-syntax (check-assembles? $asm $bytevector)
  (check (equal? (asm-bytevector $asm) $bytevector)))
