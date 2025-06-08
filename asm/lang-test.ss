(import (micascheme) (asm lang) (asm block) (asm fragment))

(define-keywords empty fragment-1 fragment-2 fragment-3 main)

(define-property empty fragment
  (fragment
    '()
    (u8-block)))

(define-property fragment-1 fragment
  (fragment
    '()
    (u8-block 10)))

(define-property fragment-2 fragment
  (fragment
    '()
    (u8-block 20 30)))

(define-property fragment-3 fragment
  (fragment
    (list #'fragment-2 #'fragment-1)
    (u8-block 40 #'fragment-2 #'fragment-1)))

(define-property main fragment
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
