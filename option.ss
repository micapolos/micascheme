(library (option)
  (export option)
  (import (scheme) (syntax) (monad-syntax))

  (define (option $value)
    (or $value
      (error 'option "option can not be #f")))

  (define-pure (option $value) (option $value))

  (define-bind (option $fn $option)
    (and $option ($fn $option)))
)
