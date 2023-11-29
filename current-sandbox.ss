(import (micascheme))

(keep-pretty-printing-current
  (lets
    (in current
      ($percent (current-random-below 100))
      (current
        (string-append
          (number->string $percent) "%")))))
