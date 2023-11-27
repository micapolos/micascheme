(library (number)
  (export pi pi2 fract)
  (import (scheme))

  (define pi (* (asin 1) 2))
  (define pi2 (* (asin 1) 4))

  (define (fract $number)
    (- $number (floor $number)))
)
