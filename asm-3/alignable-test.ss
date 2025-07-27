(import (asm-3 base) (asm-3 aligned) (asm-3 alignable))

(check (equal? (alignable 123) (aligned 1 123)))
(check (equal? (alignable (aligned 4 123)) (aligned 4 123)))
