(import (asm data))

(check-data (db 10 20 30) (db 10 20 30))
(check-data (dw #x1234 #x5678) (db #x34 #x12 #x78 #x56))
