(import (micascheme) (tico type) (tico compiler))

(check
  (equal?
    (datum->compiled 128)
    (compiled 1 2 3)))
