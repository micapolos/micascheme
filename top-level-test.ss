(import (scheme) (check) (top-level))

(check
  (equal?
    (top-level
      (import (micascheme))
      (identity "foo"))
    "foo"))
