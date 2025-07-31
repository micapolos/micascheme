(import (asm lang) (asm ops))

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

(check-asm
  (org #xc000)
  (ds 8)
  (asm (start 49152) (db 0 0 0 0 0 0 0 0)))

(check-asm
  (org #xc000)
  (ds 8 #x10)
  (asm (start 49152) (db #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x10)))
