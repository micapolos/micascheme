(import (scheme) (check) (sourced) (list))

(check
  (equal?
    (values->list (sourced (+ 2 2)))
    '(4 (+ 2 2) "sourced-test.ss" 5 28)))
