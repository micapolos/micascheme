(import (asm-3 base) (asm-2 alignment))

(check (equal? (empty-alignment) 1))
(check (equal? (alignment-append) 1))
(check (equal? (alignment-append 4 8 1 8) 8))
