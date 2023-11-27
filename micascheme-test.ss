(import (micascheme))

(check
  (equal?
    (lets
      ((cons x y) (cons "a" "b"))
      (string-append x y))
    "ab"))
