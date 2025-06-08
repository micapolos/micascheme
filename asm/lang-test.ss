(import (micascheme) (asm lang) (asm block) (asm fragment))

(define-asm empty
  (fragment
    '()
    (u8-block)))

(define-asm fragment-1
  (fragment
    '()
    (u8-block 10)))

(define-asm fragment-2
  (fragment
    '()
    (u8-block 20 30)))

(define-asm fragment-3
  (fragment
    (list #'fragment-2 #'fragment-1)
    (u8-block 40 #'fragment-2 #'fragment-1)))

(define-asm main
  (fragment
    (list #'fragment-3 #'fragment-1)
    (u8-block 50 #'fragment-1 #'fragment-3)))

(check
  (equal?
    (asm-bytevector empty 100)
    (bytevector)))

(check
  (equal?
    (asm-bytevector main 100)
    (bytevector 50 108 103 40 106 108 20 30 10)))
