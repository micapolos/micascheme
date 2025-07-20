(import (asm-3 base) (asm-2 aligned) (asm-2 alignable))

(check (equal? (alignable 123) (aligned 1 123)))
(check (equal? (alignable (aligned 4 123)) (aligned 4 123)))
