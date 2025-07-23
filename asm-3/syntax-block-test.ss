(import (asm-3 base) (asm-3 block) (asm-3 syntax-expression) (asm-3 syntax-block) (syntax lookup))

(define lookup
  (lookup-with
    (db
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          ((_ x)
            (u8-expression-block
              (syntax->expression $lookup #'x))))))))

(check-syntax->block lookup #xc000 (empty-lookup)
  foo
  (block (alignment 1) (size 0) (labels (foo #xc000)) (blobs)))

(check-syntax->block lookup #xc000 (empty-lookup)
  (label foo)
  (block (alignment 1) (size 0) (labels (foo #xc000)) (blobs)))

(check-syntax->block lookup #xc000 (empty-lookup)
  (align 4)
  (block (alignment 4) (size 0) (labels) (blobs)))

(check-syntax->block lookup #xc000 (empty-lookup)
  (db 10)
  (block (alignment 1) (size 1) (labels) (blobs (dependent (binary 10)))))

(check-syntax->block lookup #xc000 (lookup-with (foo 10))
  (db foo)
  (block (alignment 1) (size 1) (labels) (blobs (dependent (foo) (binary 10)))))

(check-syntax->block lookup #xc000 (empty-lookup)
  (begin (db 10) (db 20) (db 30))
  (block (alignment 1) (size 3) (labels)
    (blobs
      (dependent (binary 10))
      (dependent (binary 20))
      (dependent (binary 30)))))
