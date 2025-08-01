(library (asm lang-test-write)
  (export write write-op)
  (import (asm lang))

  (define-fragment write (db 201))
  (define-op (write-op) (dw write))
)
