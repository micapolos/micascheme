(import (scheme) (syntax) (check) (binder) (lets))

(define-keyword number-and-string)

(define-bind number-and-string
  (syntax-rules ()
    ((_ ($var $number $string) $body)
      (lets
        ($number $var)
        ($string (number->string $var))
        $body))))
