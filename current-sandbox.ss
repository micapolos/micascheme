(import (micascheme))

(loop-current
  (lets
    (in current
      ($percent (current-random-below 100))
      (pretty-print-current
        (current
          (string-append
            (number->string $percent) "%"))))))
