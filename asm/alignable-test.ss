(import (asm base) (asm aligned) (asm alignable))

(check (equal? (alignable 123) (aligned 1 123)))
(check (equal? (alignable (aligned 4 123)) (aligned 4 123)))
