(library (asm lang-test-library)
  (export library-ds-4-5 use-write library-write-op)
  (import (asm lang) (asm ops) (asm lang-test-write))

  (define-asm library-ds-4-5 (ds 4 5))

  (define-asm use-write
    (dw write))

  (define-op (library-write-op)
    (write-op))
)
