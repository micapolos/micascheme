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
    (call-with-bytevector-output-port
      (lambda ($port)
        ((asm-put-proc empty) $port 100)))
    (bytevector)))

(check
  (equal?
    (call-with-bytevector-output-port
      (lambda ($port)
        ((asm-put-proc main) $port 100)))
    (bytevector 20 30 10 40 100 102 50 102 103)))
