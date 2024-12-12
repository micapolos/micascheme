(import (micascheme) (z80 asm-blob) (z80 asm-syntax))

(define-asm-syntax db
  (lambda ($asm $db)
    (syntax-case $db ()
      ((db expr)
        (fluent $asm
          (asm-with-blobs (push (asm-blobs $asm) #`(u8-blob expr)))
          (asm-with-org (+ (asm-org $asm) 1)))))))

(define-asm-syntax (dw $asm $dw)
  (syntax-case $dw ()
    ((dw expr)
      (fluent $asm
        (asm-with-blobs
          (push (asm-blobs $asm)
            #`(bytevector->blob (u16-bytevector expr (endianness little)))))
        (asm-with-org (+ (asm-org $asm) 2))))))

(define-asm-syntax (zeros n)
  ($asm
    (fluent $asm
      (asm-with-blobs
        (push (asm-blobs $asm)
          #`(bytevector->blob (make-bytevector n 0))))
      (asm-with-org (+ (asm-org $asm) (datum n))))))

(check-equal?
  (asm-bytevector (db #x10))
  (bytevector #x10))

(check-equal?
  (asm-bytevector (dw #x1234))
  (bytevector #x34 #x12))

(check-equal?
  (asm-bytevector (zeros 5))
  (bytevector 0 0 0 0 0))
