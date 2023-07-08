(library (variable)
  (export variable variable? variable-index v0 v1 v2)
  (import (micascheme))

  (data (variable index))

  (define v0 (variable 0))
  (define v1 (variable 1))
  (define v2 (variable 2))
)