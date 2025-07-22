(import (except (asm-3 base) begin) (asm-3 block) (asm-3 org) (asm-3 block-syntax))

(check-block #xc000
  (empty-lookup)
  (db 10)
  (block
    (alignment 1)
    (size 1)
    (labels)
    (blobs
      (dependent (binary 10)))))

(check-block #xc000
  (empty-lookup)
  (dw-le org)
  (block
    (alignment 1)
    (size 2)
    (labels)
    (blobs
      (dependent (binary #x00 #xc0)))))

(check-block #xc000
  (empty-lookup)
  (dw-be org)
  (block
    (alignment 1)
    (size 2)
    (labels)
    (blobs
      (dependent (binary #xc0 #x00)))))

(check-block #xc000
  (empty-lookup)
  (begin
    (label start)
    (db 10)
    (align 2)
    (dw-le #x1234)
    (dw-le org)
    (label end))
  (block
    (alignment 2)
    (size 6)
    (labels
      (start #xc000)
      (end #xc006))
    (blobs
      (dependent (binary 10))
      (dependent (binary 0))
      (dependent (binary #x34 #x12))
      (dependent (binary #x04 #xc0)))))
