(import (zx-next test) (zx-next scheme alloc))

(test
  (case alloc-1
    (byte-alloc 6)
    (assert e #x01)
    (assert hl #xe003)))
