(import (zx-next test) (zx-next bit-index))

(test
  (case load-bit-index
    (load-bit-index #xe000 #x75)
    (assert hl #xe00e)
    (assert e #b00100000)))
