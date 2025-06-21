(import
  (micascheme)
  (asm asm)
  (asm block)
  (asm binary))

(define-asm (db $lookup $syntax)
  (syntax-case $syntax ()
    ((_ expr)
      (lambda ($block)
        (block+binary-syntax-proc $block 1
          (lambda ($org) #'(db-binary expr)))))))

(define-asm (dw $lookup $syntax)
  (syntax-case $syntax ()
    ((_ expr)
      (lambda ($block)
        (block+binary-syntax-proc $block 2
          (lambda ($org) #'(dw-binary expr)))))))

(define-asm (label $lookup $syntax)
  (syntax-case $syntax ()
    ((_ x)
      (lambda ($block)
        (block+label $block #'x)))))

(define-asm-rules
  ((ret) (db #xc9))
  ((jp nn) (db #xff) (dw nn)))

(check
  (equal?
    (asm-bytevector)
    (bytevector)))

(check
  (equal?
    (asm-bytevector (db #x12))
    (bytevector #x12)))

(check
  (equal?
    (asm-bytevector (dw #x1234))
    (bytevector #x34 #x12)))

(check
  (equal?
    (asm-bytevector (db #x12) (db #x34))
    (bytevector #x12 #x34)))

(check
  (equal?
    (asm-bytevector (label begin) (db begin) (db end) (label end))
    (bytevector 0 2)))

(check
  (equal?
    (asm-bytevector (org 100) (label begin) (db begin) (db end) (label end))
    (bytevector 100 102)))

(check
  (equal?
    (asm-bytevector
      (org #x20)
      (label loop)
      (jp loop)
      (ret))
    (bytevector #xff #x20 0 #Xc9)))
