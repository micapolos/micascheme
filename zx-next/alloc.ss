(library (zx-next alloc)
  (export)
  (import (micascheme))

  (define-asm alloc
    (input (bc size))
    (output (hl pointer) (cf out-of-memory)))
)
