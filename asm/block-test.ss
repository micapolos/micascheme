(import (micascheme) (asm typed) (asm block) (syntax lookup))

(define-rules-syntax (literals org)
  ((check-block (org $org) block stx)
    (check-datum=?
      (block-binary-syntax block $org)
      'stx))
  ((check-block . args)
    (check-block (org 0) . args)))

(check-block (org 100)
  (block
    2
    (stack
      (cons #'a 10)
      (cons #'b 20))
    (stack
      #`(db-binary a)
      #`(db-binary b)))
  (let
    ((a 110) (b 120))
    (binary-append
      (db-binary a)
      (db-binary b))))

(check-block (org 100)
  (fluent (empty-block)
    (block+label #'pre)
    (block+binary-syntax 2 #'(pre-op))
    (block+local
      (fluent (empty-block)
        (block+label #'local)
        (block+binary-syntax 3 #'(local-op))))
    (block+label #'post)
    (block+binary-syntax 2 #'(post-op))
    (block+label #'end))
  (let
    ((pre 100) (post 105) (end 107))
    (binary-append
      (pre-op)
      (let ((local 2)) (binary-append (local-op)))
      (post-op))))
