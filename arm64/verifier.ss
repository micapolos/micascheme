(library (arm64 verifier)
  (export
    make-verifier
    verifier?
    verifier-state

    verifier-state-error?
    verifier-state-start?

    verifier-emit-a64
    verifier-emit-va64)
  (import (scheme) (throw))

  (define-record-type verifier
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-verifier-state-start)))))
    (fields
      (mutable state)))

  (define-record-type verifier-state-start
    (fields))

  (define-record-type verifier-state-error
    (fields))

  (define (verifier-emit-a64 $verifier $op)
    (unless (verifier-state-error? (verifier-state $verifier))
      (verifier-state-set! $verifier (make-verifier-state-error))
      (throw error)))

  (define (verifier-emit-va64 $verifier $op)
    (unless (verifier-state-error? (verifier-state $verifier))
      (verifier-state-set! $verifier (make-verifier-state-error))
      (throw error)))
)
