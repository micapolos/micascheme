(library (boolean)
  (export false? not-false? xor)
  (import (scheme))

  (define (false? $value) (not $value))
  (define (not-false? $value) (not (false? $value)))

  (define-syntax xor
    (syntax-rules ()
      ((_) #f)
      ((_ $a) $a)
      ((_ $a $b ...) (if $a (and (not $b) ...) (xor $b ...)))))
)
