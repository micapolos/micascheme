(import (leo-print))

(leo-print
  '(
    10 foo "bar"
    (point (x 10) (y 20))
    (circle
      (radius 10)
      (center (point (x 20) (y 30))))))
