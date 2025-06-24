(import (micascheme) (typico typed) (typico literal-expand))

(define-rule-syntax (check-literal-expand in out)
  (check (equal? (typed->datum (literal-expand-typed #'in)) 'out)))

(check-literal-expand #f (typed boolean #f))
(check-literal-expand 123 (typed integer 123))
(check-literal-expand #\a (typed char #\a))
(check-literal-expand "foo" (typed string "foo"))
