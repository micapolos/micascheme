(library (asm lookable)
  (export lookable lookable-ref)
  (import (micascheme))

  (define-rules-syntax
    ((lookable body)
      (lookable (_) body))
    ((lookable ($lookup) body)
      (lambda ($lookup) body)))

  (define (lookable-ref $lookable $lookup)
    ($lookable $lookup))
)
