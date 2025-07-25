(import (asm-3 lang) (asm-3 ops))

(check-asm
  (org #xc000)
  (dup 3
    (with-labels (start end)
      start
      (dw start #xffff end)
      end))
  (asm
    (start #xc000)
    (db
      #x00 #xc0 #xff #xff #x06 #xc0
      #x06 #xc0 #xff #xff #x0c #xc0
      #x0c #xc0 #xff #xff #x12 #xc0)))
