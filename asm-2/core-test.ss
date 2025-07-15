(import (micascheme) (asm-2 block-fragment) (asm-2 core) (syntax lookup))

(check
  (equal?
    (fragment->bytevector
      (block
        start
        (db #x12)
        (dw #x3456)
        (dw start))
      (empty-lookup)
      #xc000)
    (bytevector #x12 #x56 #x34 #x00 #xc0)))
