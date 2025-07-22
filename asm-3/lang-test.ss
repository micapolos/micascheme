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

(check-linked
  (org #xc000)
  (db 10 20 30)
  (linked
    (start #xc000)
    (db 10 20 30)))

; (check-linked
;   (org #xc000)
;   (dw-be foo)
;   (linked
;     (start #xc000)
;     (db 10 10 20)))

; (check-linked
;   (org #xc000)
;   (dw-le main)
;   (linked
;     (start #xc000)
;     (db 10 0 20 #x34 #x12 #xc0 #x05)))
