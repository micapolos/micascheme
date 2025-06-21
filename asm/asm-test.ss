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
