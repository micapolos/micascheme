(import (micascheme))

(run-current
  (lets (in current)
    ($a (print-current current-seconds))
    (print-current current-random)))
