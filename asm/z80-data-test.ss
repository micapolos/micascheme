(import (asm data) (asm z80-data))

(check-data (halt) (db #b10110110))

; (check-data (and h) (db #b10100100))
; (check-data (and (hl)) (db #b10100101))
(check-data (and (+ ix 2)) (db #xdd #b10100101 #x02))

