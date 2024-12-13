(import (micascheme) (asm) (asm syntax))

(define-asm-syntax db
  (lambda ($asm $db)
    (syntax-case $db ()
      ((db expr)
        (fluent $asm
          (asm-with-blobs (push (asm-blobs $asm) #`(u8-blob expr)))
          (asm+org 1))))))

(define-asm-syntax (dw $asm $dw)
  (syntax-case $dw ()
    ((dw expr)
      (fluent $asm
        (asm-with-blobs
          (push (asm-blobs $asm)
            #`(bytevector->blob (u16-bytevector expr (endianness little)))))
        (asm+org 2)))))

(define-asm-syntax (zeros n) ($asm)
  (fluent $asm
    (asm-with-blobs (push (asm-blobs $asm) #`(bytevector->blob (make-bytevector n 0))))
    (asm-with-org (+ (asm-org $asm) (datum n)))))

(define-asm-syntax (eq id expr) ($asm)
  (asm-with-values $asm
    (push (asm-values $asm)
      #`(id expr))))

(check-equal?
  (asm-bytevector (db #x10))
  (bytevector #x10))

(check-equal?
  (asm-bytevector (dw #x1234))
  (bytevector #x34 #x12))

(check-equal?
  (asm-bytevector (zeros 5))
  (bytevector 0 0 0 0 0))

(check-equal?
  (asm-bytevector (db foo) (db #x10) foo (db foo) (zeros 3) (db (+ foo foo)))
  (bytevector #x02 #x10 #x02 0 0 0 #x04))

(check-equal?
  (asm-bytevector (eq foo+3 (+ foo 3)) (zeros 5) foo (db foo+3))
  (bytevector 0 0 0 0 0 8))
