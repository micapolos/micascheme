(import (micascheme) (tico term))

(check
  (equal?
    (term-argument-arity (variable-term (index 2)))
    1))
