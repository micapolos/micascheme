(library (curry)
  (export curry< curry+ curry-)
  (import (scheme))

  (define (curry< a)
    (lambda (b)
      (< a b)))

  (define (curry+ a)
    (lambda (b)
      (+ a b)))

  (define (curry- a)
    (lambda (b)
      (- a b)))
)
