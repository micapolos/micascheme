(import (asm lang) (only (micascheme) syntax-rules) (asm ops))

(define val-10 10)
(define val-20 20)
(define val-30 (+ val-10 val-20))

(define-op (ret) (db 201))

(define-ops (keywords a b)
  ((ld a b)  (db 101))
  ((ld a n)  (db 62) (db n)))

(define-asm proc-10 (db 10))
(define-asm proc-20 (db 20))
(define-asm ds-4-5 (ds 4 5))

(define-asm main
  (align 2)
  (dw proc-10)
  (dw proc-20))

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
  (db val-20)
  (asm
    (start #xc000)
    (db 20)))

(check-asm
  (org #xc000)
  (db (+ val-10 val-20))
  (asm
    (start #xc000)
    (db 30)))

(check-asm
  (org #xc000)
  (dz "foo")
  (asm
    (start #xc000)
    (db 102 111 111 0)))

(check-asm
  (org #xc000)
  start
  (db-e start)
  (db-e start)
  (db-e start)
  (db-e end)
  (db-e end)
  (db-e end)
  end
  (asm
    (start #xc000)
    (db 255 254 253 2 1 0)))

(check-asm
  (org #xc000)
  (with-labels (foo)
    (dw foo)
    foo)
  (asm
    (start #xc000)
    (db #x02 #xc0)))

(check-asm
  (org #xc000)
  (db val-30)
  (asm
    (start #xc000)
    (db 30)))

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
  (reverse
    (db 10)
    (db 20)
    (db 30))
  (asm
    (start #xc000)
    (db 30 20 10)))

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
  (dw (+ ds-4-5 2))
  (asm
    (start #xc004)
    (db 5 5 5 5 #x02 #xc0)))
