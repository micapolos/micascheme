(import (micascheme) (asm) (asm syntax))

(check-equal?
  (asm-bytevector (u8 #x10 #x20))
  (bytevector #x10 #x20))

(check-equal?
  (asm-bytevector
    (eq one 1)
    (eq two (+ one one))
    (u8 one two))
  (bytevector 1 2))

(check-equal?
  (asm-bytevector
    (u8 10 20 label-1 label-2)
    label-1
    (u8 30 40)
    label-2)
  (bytevector 10 20 4 6 30 40))

(check-equal?
  (asm-bytevector
    (org #x12)
    here
    (u8 here))
  (bytevector #x12))

(run
  (define-asm-syntax db
    (lambda ($asm $db)
      (syntax-case $db ()
        ((db expr) (asm+u8 $asm #`expr)))))

  (check-equal?
    (asm-bytevector (db #x10))
    (bytevector #x10)))

(run
  (define-asm-syntax (db $asm $u8)
    (syntax-case $u8 ()
      ((db expr) (asm+u8 $asm #`expr))))

  (check-equal?
    (asm-bytevector (db #x10))
    (bytevector #x10)))

(run
  (define-asm-syntax (db expr) ($asm)
     (asm+u8 $asm #`expr))

  (check-equal?
    (asm-bytevector (db #x10))
    (bytevector #x10)))
