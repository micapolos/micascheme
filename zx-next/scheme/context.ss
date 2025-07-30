(library (zx-next scheme context)
  (export context-preserve)
  (import (zx-next core))

  (define-op (context-preserve body ...)
    (stack-preserve (value-preserve body ...)))
)
