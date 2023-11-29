(import (micascheme))

(pretty-print-current
  (current-lets
    ($percent (current-random-below 100))
    (current
      (string-append
        (number->string $percent) "%"))))
