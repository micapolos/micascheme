(import (leo2 base) (leo2 term) (leo2 solver))

(check-solver=?
  (solver (native "foo"))
  (solver (native "foo")))
