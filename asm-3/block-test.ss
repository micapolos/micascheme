(import (asm-3 base) (asm-3 block) (asm-3 expression) (syntax lookup))

(check-block #xc000
  (empty-lookup)
  (empty-block)
  (block (alignment 1) (size 0) (labels) (blobs)))

(check-block 3
  (empty-lookup)
  (align-block 8)
  (block (alignment 8) (size 0) (labels) (blobs)))

(check-block #xc000
  (empty-lookup)
  (u8-block 100)
  (block (alignment 1) (size 1) (labels) (blobs (dependent (binary 100)))))

(check-block #xc000
  (empty-lookup)
  (u16-block #x1234 (endianness big))
  (block (alignment 1) (size 2) (labels) (blobs (dependent (binary #x12 #x34)))))

(check-block #xc000
  (empty-lookup)
  (u16-block #x1234 (endianness little))
  (block (alignment 1) (size 2) (labels) (blobs (dependent (binary #x34 #x12)))))

(check-block #xc000
  (empty-lookup)
  (bytevector-block (bytevector 10 20 30))
  (block (alignment 1) (size 3) (labels) (blobs (dependent (binary 10 20 30)))))

(check-block #xc000
  (empty-lookup)
  (u8-expression-block (pure-expression 123))
  (block (alignment 1) (size 1) (labels) (blobs (dependent (binary 123)))))

(check-block #xc000
  (lookup-with (foo 123))
  (u8-expression-block (identifier-expression #'foo))
  (block (alignment 1) (size 1) (labels) (blobs (dependent (foo) (binary 123)))))

(check-block #xc000
  (empty-lookup)
  (u16-expression-block (pure-expression #x1234) (endianness big))
  (block (alignment 1) (size 2) (labels) (blobs (dependent (binary #x12 #x34)))))

(check-block #xc000
  (empty-lookup)
  (u16-expression-block (pure-expression #x1234) (endianness little))
  (block (alignment 1) (size 2) (labels) (blobs (dependent (binary #x34 #x12)))))

(check-block #xc000
  (empty-lookup)
  (u16-expression-block (org-expression) (endianness big))
  (block (alignment 1) (size 2) (labels) (blobs (dependent (binary #xc0 #x00)))))

(check-block #xc000
  (empty-lookup)
  (identifier-block #'foo)
  (block (alignment 1) (size 0) (labels (foo #xc000)) (blobs)))

(check-block #xc000
  (empty-lookup)
  (block-append)
  (block (alignment 1) (size 0) (labels) (blobs)))

(check-block #xc000
  (lookup-with (foo 10) (bar 20))
  (block-append
    (identifier-block #'start)
    (u8-expression-block (identifier-expression #'foo))
    (align-block 4)
    (u8-expression-block (identifier-expression #'bar))
    (align-block 2)
    (u16-expression-block (org-expression) (endianness big))
    (identifier-block #'end))
  (block
    (alignment 4)
    (size 8)
    (labels (start #xc000) (end #xc008))
    (blobs
      (dependent (foo) (binary 10))
      (dependent (binary 0 0 0))
      (dependent (bar) (binary 20))
      (dependent (binary 0))
      (dependent (binary #xc0 #x06)))))

(check-block #xc000
  (empty-lookup)
  (block-append
    (identifier-block #'start)
    (u16-expression-block (identifier-expression #'start) (endianness big))
    (u16-expression-block (org-expression) (endianness big))
    (u16-expression-block (identifier-expression #'end) (endianness big))
    (identifier-block #'end))
  (block (alignment 1) (size 6)
    (labels
      (start #xc000)
      (end #xc006))
    (blobs
      (dependent (start) (binary #xc0 #x00))
      (dependent (binary #xc0 #x02))
      (dependent (end) (binary #xc0 #x06)))))
