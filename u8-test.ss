(import (micascheme) (u8))

(check (equal? (fx->u8 #x1234) #x34))
(check (equal? (u8+ #x90 #x90) #x20))
(check (equal? (u8- #x10 #x20) #xf0))
