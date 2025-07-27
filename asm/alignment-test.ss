(import (asm base) (asm alignment))

(check (equal? (empty-alignment) 1))
(check (equal? (alignment-append) 1))
(check (equal? (alignment-append 4 8 1 8) 8))
