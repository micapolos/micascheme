(import (check) (binder))

(define-binder number-and-string
  (lambda ($number $fn)
    ($fn $number (number->string $number))))
