(import (micascheme) (asm typed) (asm block) (syntax lookup))

(define-rules-syntax (literals org)
  ((check-block block stx)
    (check-datum=?
      (block-binary-syntax block)
      'stx)))

(check-block
  (block 100
    (stack
      (cons #'a 110)
      (cons #'b 120))
    (stack
      #`(db-binary a)
      #`(db-binary b))
    #'()
    (stack))
  (let
    ((a 110) (b 120))
    (binary-append (db-binary a) (db-binary b))))

(check-block
  (fluent (empty-block 100)
    (block+label #'pre)
    (block+binary-syntax 2 #'(pre-op))
    (block-bind
      (lambda ($block)
        (fluent $block
          (block+label #'local)
          (block+binary-syntax 3 #'(local-op)))))
    (block+label #'post)
    (block+binary-syntax 2 #'(post-op))
    (block+label #'end))
  (let
    ((pre 100) (post 105) (end 107))
    (binary-append
      (pre-op)
      (let ((local 102)) (local-op))
      (post-op))))

(check
  (equal?
    (block->map-string
      (block-with-labels (empty-block 100)
        (stack
          (cons #'foo-bar #x1234)
          (cons #'goo->zar/gar #x2345))))
    (lines-string
      "1234 foo-bar"
      "2345 goo->zar/gar")))
