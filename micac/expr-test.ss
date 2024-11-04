(import (micascheme) (micac expr) (code))

(check
  (equal?
    (code-string
      (expr-code
        (binary-expr 2 #t
          (expr 1 #f (code "a"))
          " + "
          (expr 1 #f (code "b")))))
    "a + b"))
