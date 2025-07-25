(import (asm-3 lang) (asm-3 ops))

;(trace-block-expansion #t)

; (check-asm
;   (org #xc000)
;   (dup 3
;     (with-labels (start end)
;       start
;       (dw start 0 end)
;       end))
;   (asm
;     (start #xc000)
;     (db #x34 #x12 #x78 #x56)))
