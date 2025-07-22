(import (asm-3 lang))

(const val-10 10)

(data foo
  (db 10)
  (db val-10)
  (db (+ 10 val-10)))

(proc main
  (db 10)
  (align 2)
  (db 20)
  (begin
    (dw-le #x1234)
    (dw-be org)))

(check-assembled
  (org #xc000)
  (db 10 20 30)
  (assembled
    (start #xc000)
    (db 10 20 30)))

; (check-assembled
;   (org #xc000)
;   (dw-be foo)
;   (assembled
;     (start #xc000)
;     (db 10 10 20)))

; (check-assembled
;   (org #xc000)
;   (dw-le main)
;   (assembled
;     (start #xc000)
;     (db 10 0 20 #x34 #x12 #xc0 #x05)))
