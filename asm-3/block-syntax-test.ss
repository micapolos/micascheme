(import (except (asm-3 base) begin) (asm-3 block) (asm-3 org) (asm-3 block-syntax))

(check-block #xc000
  (empty-lookup)
  (u8 10)
  (block
    (alignment 1)
    (size 1)
    (labels)
    (blobs
      (dependent (binary 10)))))

(check-block #xc000
  (empty-lookup)
  (u16 org (endianness little))
  (block
    (alignment 1)
    (size 2)
    (labels)
    (blobs
      (dependent (binary #x00 #xc0)))))

(check-block #xc000
  (empty-lookup)
  (begin
    (label start)
    (u8 10)
    (align 2)
    (u16 #x1234 (endianness little))
    (u16 org (endianness little))
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
