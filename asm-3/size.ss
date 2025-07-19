(library (asm-3 size)
  (export empty-size size-append list->size)
  (import (asm-3 base))

  (define-monoid (size 0 +))
)
