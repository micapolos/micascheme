(import (micascheme) (asm typed) (asm lookable) (asm-2 relocable) (asm block) (syntax lookup))

(check-block
  (lookup-with
    (foo #'"foo")
    (bar #'"bar"))
  100
  (block 30
    (stack
      (cons #'a 10)
      (cons #'b 20))
    (stack
      (lookable (relocable-with #`(db-binary a)))
      (lookable ($lookup) (relocable-with #`(db-binary #,($lookup #'foo) #,($lookup #'bar))))
      (lookable (relocable-with ($org) #`(db-binary #,(literal->syntax (+ $org 2))))))
    #'(asm test)
    (stack #'(lib a) #'(lib b)))
  (block (asm test)
    (import (lib a) (lib b))
    (let
      ((a 110) (b 120))
      (binary-append
        (db-binary a)
        (db-binary "foo" "bar")
        (db-binary 102)))))

(check-block
  (lookup-with)
  100
  (fluent (empty-block)
    (block-with-import-base #'(asm test))
    (block+label #'pre)
    (block+lookable-relocable-binary-syntax 2
      (lookable (relocable-with #'(pre-op))))
    (block-bind
      (lambda ($block)
        (fluent $block
          (block+label #'local)
          (block+lookable-relocable-binary-syntax 3
            (lookable (relocable-with #'(local-op)))))))
    (block+label #'post)
    (block+lookable-relocable-binary-syntax 3
      (lookable (relocable-with #'(post-op))))
    (block+label #'end))
  (block (asm test)
    (import)
    (let
      ((pre 100) (post 105) (end 108))
      (binary-append
        (pre-op)
        (let ((local 102)) (local-op))
        (post-op)))))

(check
  (equal?
    (block->map-string
      (block-with-labels (empty-block)
        (stack
          (cons #'foo-bar #x1234)
          (cons #'goo->zar/gar #x2345))))
    (lines-string
      "00001234 00001234 00 foo-bar"
      "00002345 00002345 00 goo->zar/gar")))
