(import (micascheme) (asm) (asm syntax))

(check-equal?
  (asm-bytevector (db #x10 #x20))
  (bytevector #x10 #x20))

(check-equal?
  (asm-bytevector
    (eq one 1)
    (eq two (+ one one))
    (db one two))
  (bytevector 1 2))

(check-equal?
  (asm-bytevector
    (db 10 20 label-1 label-2)
    label-1
    (db 30 40)
    label-2)
  (bytevector 10 20 4 6 30 40))

(check-equal?
  (asm-bytevector
    (org #x12)
    here
    (db here))
  (bytevector #x12))

(run
  (define-asm-syntax u8
    (lambda ($asm $db)
      (syntax-case $db ()
        ((u8 expr)
          (fluent $asm
            (asm+blob #`(u8-blob expr))
            (asm+org 1))))))

  (check-equal?
    (asm-bytevector (u8 #x10))
    (bytevector #x10)))

(run
  (define-asm-syntax (u8 $asm $u8)
    (syntax-case $u8 ()
      ((u8 expr)
        (fluent $asm
          (asm+blob #`(u8-blob expr))
          (asm+org 1)))))

  (check-equal?
    (asm-bytevector (db #x10))
    (bytevector #x10)))

(run
  (define-asm-syntax (u8 expr) ($asm)
    (fluent $asm
      (asm+blob #`(u8-blob expr))
      (asm+org 1)))

  (check-equal?
    (asm-bytevector (db #x10))
    (bytevector #x10)))
