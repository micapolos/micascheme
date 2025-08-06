(import (zx-next test) (zx-next alloc))

(test
  (case alloc-1
    (alloc 6)
    (assert e #x01)
    (assert hl #xe003))

  (case alloc-2
    (alloc 6)
    (assert e #x01)
    (assert hl #xe00b)))
