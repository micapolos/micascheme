(library (arm64 verifier)
  (export
    make-verifier
    verifier?
    verifier-error?
    verifier-error?-set!

    verifier-emit-a64
    verifier-emit-va64)
  (import (scheme) (throw))

  (define-record-type verifier
    (fields
      (mutable error?)))

  (define (verifier-emit-a64 $verifier $op)
    (unless (verifier-error? $verifier)
      (verifier-error?-set! $verifier #t)
      (throw error)))

  (define (verifier-emit-va64 $verifier $op)
    (unless (verifier-error? $verifier)
      (verifier-error?-set! $verifier #t)
      (throw error)))
)
