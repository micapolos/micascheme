(import (micascheme) (nex))

(check
  (equal?
    (blob-size (nex-blob (u8-blob 1 2 3)))
    (+ 512 #x4000)))
