(library (string)
  (export lines-string)
  (import (scheme) (list-syntax))

  (define (lines-string . $lines)
    (apply string-append
      (map-with ($line $lines)
        (string-append $line "\n"))))
)
