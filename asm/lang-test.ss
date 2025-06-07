(import (micascheme) (asm lang) (asm block) (asm fragment))

(define-keywords fragment-1 fragment-2 fragment-3 main)

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
    (u8-block 40 50 60)))

(define-property main fragment
  (fragment
    (list #'fragment-3 #'fragment-1)
    (u8-block #'fragment-3)))

(displayln (call-with-bytevector-output-port (asm-put-proc 100 main)))
