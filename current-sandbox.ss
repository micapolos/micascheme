(import (micascheme))

(pretty-print-current
  (lets
    ((current $percent) (current-random-below 100))
    (current
      (string-append
        (number->string $percent) "%"))))
