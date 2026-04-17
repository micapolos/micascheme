(import (scheme) (check) (sourced) (list))

(let-values (((procedure datum sources) (sourced (+ 2 2))))
  (check (equal? (procedure) 4))
  (check (equal? datum '(+ 2 2)))
  (check (equal? sources '((path "sourced-test.ss") (at (line 3) (column 50))))))
