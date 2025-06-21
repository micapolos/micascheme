(import (micascheme) (asm typed) (asm block) (syntax lookup))

(define-rules-syntax (literals org)
  ((check-block (org $org-identifier) block stx)
    (check-datum=?
      (block-binary-syntax block #'$org-identifier)
      'stx)))

(check-block (org 100)
  (block
    2
    (stack
      (cons #'a 10)
      (cons #'b 20))
    (stack
      (lambda ($org-identifier) #`(db-binary a))
      (lambda ($org-identifier) #`(db-binary b))))
  (let
    ((a (%+ 100 10)) (b (%+ 100 20)))
    (binary-append (db-binary a) (db-binary b))))

(check-block (org 100)
  (fluent (empty-block)
    (block+label #'pre)
    (block+binary-syntax-proc 2 (lambda ($org) #'(pre-op)))
    (block+local
      (fluent (empty-block)
        (block+label #'local)
        (block+binary-syntax-proc 3 (lambda ($org) #'(local-op)))))
    (block+label #'post)
    (block+binary-syntax-proc 2 (lambda ($org) #'(post-op)))
    (block+label #'end))
  (let
    ((pre (%+ 100 0)) (post (%+ 100 5)) (end (%+ 100 7)))
    (binary-append
      (pre-op)
      (let (($org (%+ 100 2)))
        (let ((local (%+ $org 0)))
          (binary-append (local-op))))
      (post-op))))
