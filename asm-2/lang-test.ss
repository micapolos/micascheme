(import (micascheme) (asm-2 lang) (asm-2 fragment) (asm-2 block))

(define-fragment db-10
  (fragment-with
    (block 1
      (lambda ($org)
        (u8-binary 10)))))

(define-fragment db-20
  (fragment-with
    (block 1
      (lambda ($org)
        (u8-binary 20)))))

(define-fragment db-org
  (fragment-with
    (block 1
      (lambda ($org)
        (u8-binary $org)))))

(define-fragment db-dep-10
  (fragment-with
    (dep db-10)
    (block 1
      (lambda ($org)
        (u8-binary (dep db-10))))))

(check
  (equal?
    (fragment-bytevector db-10 100)
    (bytevector 10)))

(check
  (equal?
    (fragment-bytevector db-20 100)
    (bytevector 20)))

(check
  (equal?
    (fragment-bytevector db-org 100)
    (bytevector 100)))

; (check
;   (equal?
;     (fragment-bytevector db-dep-10 100)
;     (bytevector 100)))
