(import (asm-3 lang))

(const val-10 10)

; (proc main
;   (db 10)
;   (align 2)
;   (db 20))

(check-assembled
  (org #xc000)
  (db 10 20 30)
  (assembled
    (start #xc000)
    (db 10 20 30)))

(check-assembled
  (org #xc000)
  (dw #x1234 #x5678)
  (assembled
    (start #xc000)
    (db #x34 #x12 #x78 #x56)))

(check-assembled
  (org #xc000)
  (db val-10)
  (assembled
    (start #xc000)
    (db 10)))

(check-assembled
  (org #xc000)
  (db (+ val-10 20))
  (assembled
    (start #xc000)
    (db 30)))

(check-assembled
  (org #xc000)
  (begin
    (db 10)
    (db 20)
    (db 30))
  (assembled
    (start #xc000)
    (db 10 20 30)))

(check-assembled
  (org #xc000)
  (label start)
  (assembled
    (start #xc000)
    (db)))

(check-assembled
  (org #xc000)
  (begin
    (label start)
    (dw start)
    (dw end)
    (label end))
  (assembled
    (start #xc000)
    (db #x00 #xc0 #x04 #xc0)))
