(import (scheme) (check) (failure) (lets) (procedure))

(lets
  ($failure (failure "foo"))
  (run
    (check (failure? $failure))
    (check (not (failure? "foo")))
    (check (equal? (failure-value $failure) "foo"))))
