(import (micascheme) (tico term) (tico index))

(check
  (equal?
    (term-constant-arity (variable-term (index 2)))
    1))
