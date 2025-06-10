(library (property)
  (export property properties)
  (import (scheme))

  (define-syntax property
    (syntax-rules ()
      ((_ car cdr)
        (cons #'car cdr))))

  (define-syntax properties
    (syntax-rules ()
      ((_ (car cdr) ...)
        (list (property car cdr) ...))))
)
