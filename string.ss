(library (string)
  (export
    lines-string
    lines-string0)
  (import (scheme) (list) (list-syntax))

  (define (lines-string . $lines)
    (apply string-append
      (map-with ($line $lines)
        (string-append $line "\n"))))

  (define (lines-string0 . $lines)
    (apply string-append
      (intercalate $lines "\n")))
)
