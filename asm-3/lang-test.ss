(import (asm-3 lang) (only (micascheme) syntax-rules))

(define-ops (keywords a b)
  ((ret)     (db 201))
  ((ld a b)  (db 101))
  ((ld a n)  (db 62) (db n)))

(const val-10 10)

(proc proc-10 (db 10))
(proc proc-20 (db 20))
(proc main (align 2) (dw proc-10) (dw proc-20))

(check-asm
  (org #xc000)
  (db 10 20 30)
  (asm
    (start #xc000)
    (db 10 20 30)))

(check-asm
  (org #xc000)
  (dw #x1234 #x5678)
  (asm
    (start #xc000)
    (db #x34 #x12 #x78 #x56)))

(check-asm
  (org #xc000)
  (db val-10)
  (asm
    (start #xc000)
    (db 10)))

(check-asm
  (org #xc000)
  (begin
    (db 10)
    (db 20)
    (db 30))
  (asm
    (start #xc000)
    (db 10 20 30)))

(check-asm
  (org #xc000)
  (begin
    (db 10)
    (align 4)
    (db 20))
  (asm
    (start #xc000)
    (db 10 0 0 0 20)))

(check-asm
  (org #xc000)
  start
  (asm
    (start #xc000)
    (db)))

(check-asm
  (org #xc000)
  (begin
    start
    (dw start)
    (dw end)
    end)
  (asm
    (start #xc000)
    (db #x00 #xc0 #x04 #xc0)))

(check-asm
  (org #xc000)
  (ret)
  (asm
    (start #xc000)
    (db 201)))

(check-asm
  (org #xc000)
  (ld a b)
  (asm
    (start #xc000)
    (db 101)))

(check-asm
  (org #xc000)
  (ld a 16)
  (asm
    (start #xc000)
    (db 62 16)))

(check-asm
  (org #xc000)
  (dw proc-10)
  (asm
    (start #xc001)
    (db 10 #x00 #xc0)))

(check-asm
  (org #xc000)
  (dw proc-20)
  (asm
    (start #xc001)
    (db 20 #x00 #xc0)))

(check-asm
  (org #xc000)
  (dw main)
  (asm
    (start #xc006)
    (db #x04 #xc0 #x05 #xc0 10 20 #x00 #xc0)))
