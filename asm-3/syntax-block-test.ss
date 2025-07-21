(import (asm-3 base) (asm-3 syntax-block) (syntax lookup) (asm-3 org))

(check-syntax->block #xc000
  (empty-lookup)
  (db 10 20 30)
  (block (alignment 1) (size 3) (labels)
    (blobs
      (dependent (binary 10))
      (dependent (binary 20))
      (dependent (binary 30)))))

(check-syntax->block #xc000
  (empty-lookup)
  (dw #x1234 #x5678)
  (block (alignment 1) (size 4) (labels)
    (blobs
      (dependent (binary #x34 #x12))
      (dependent (binary #x78 #x56)))))

(check-syntax->block #xc000
  (empty-lookup)
  (dw org)
  (block (alignment 1) (size 2) (labels)
    (blobs
      (dependent (binary #x00 #xc0)))))

(check-syntax->block #xc000
  (empty-lookup)
  (align 4)
  (block (alignment 4) (size 0) (labels)  (blobs)))

(check-syntax->block #xc000
  (empty-lookup)
  foo
  (block (alignment 1) (size 0) (labels (foo #xc000))  (blobs)))

(check-syntax->block #xc000
  (empty-lookup)
  (begin
    (db 10)
    (db 20))
  (block (alignment 1) (size 2) (labels)
    (blobs
      (dependent (binary 10))
      (dependent (binary 20)))) )
