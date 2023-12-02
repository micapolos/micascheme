(import (micascheme) (tico term))

(check
  (equal?
    (term-constant-arity (variable-term (index 2)))
    1))
