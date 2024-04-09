(import (syntax) (check) (binder))

(define-aux-keyword number-and-string)

(define-binder number-and-string
  (lambda ($number $fn)
    ($fn $number (number->string $number))))
