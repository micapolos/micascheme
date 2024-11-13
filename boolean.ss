(library (boolean)
  (export false? not-false? xor and-proc or-proc)
  (import (scheme) (procedure))

  (define (false? $value) (not $value))
  (define (not-false? $value) (not (false? $value)))

  (define-syntax xor
    (syntax-rules ()
      ((_) #f)
      ((_ $a) $a)
      ((_ $a $b ...) (if $a (and (not $b) ...) (xor $b ...)))))

  (define (and-proc . $args) (andmap identity $args))
  (define (or-proc . $args) (ormap identity $args))
)
