(library (commented)
  (export commented)
  (import (scheme) (syntax))

  (define-rule-syntax (commented comments ... body) body)
)
