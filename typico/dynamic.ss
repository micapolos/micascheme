(library (typico dynamic)
  (export dynamic)
  (import (micascheme))

  (define-rule-syntax (dynamic x) x)
)
