(import (micascheme) (animated))

(check
  (equal?
    (animated-values (triangle-wave 0.0) 0.25 10)
    (list 0.0 0.25 0.5 0.75 0.0 0.25 0.5 0.75 0.0 0.25)))
