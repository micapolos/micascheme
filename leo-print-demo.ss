(import (leo-print))

(print-leo
  10
  foo
  #\A
  #\newline
  #t
  #f
  'foo
  #'foo
  `(foo ,bar)
  (scheme
    (ld a h)
    (ret)
    (lambda (a) a))
  #`(foo #,bar)
  "bar"
  (x 10)
  (radius (length 10))
  (point (x 10) (y 20))
  (circle
    (radius 10)
    (center (point (x 20) (y 30)))))
