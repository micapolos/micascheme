(library (arm64 verifier)
  (export
    make-verifier
    verifier?
    verifier-state
    verifier-functions

    verifier-state-error?
    verifier-state-start?
    verifier-state-function?

    verifier-emit-a64
    verifier-emit-va64)
  (import
    (scheme)
    (throw)
    (switch)
    (arm64 encode))

  (define-record-type verifier
    (protocol
      (lambda (make)
        (lambda ()
          (make
            (make-verifier-state-start)
            0
            (vector)))))
    (fields
      (mutable state)
      (mutable pc)
      (immutable functions)))

  (define-record-type verifier-state-start
    (fields))

  (define-record-type verifier-state-function
    (fields))

  (define-record-type verifier-state-error
    (fields))

  (define (verifier-emit-a64 $verifier $op)
    (unless (verifier-state-error? (verifier-state $verifier))
      (switch (verifier-state $verifier)
        ((verifier-state-function? $verifier-state-function)
          (cond
            ((nop? $op)
              (verifier-inc-pc $verifier))
            (else
              (verifier-fail $verifier))))
        ((else _)
          (verifier-fail $verifier)))))

  (define (verifier-emit-va64 $verifier $op)
    (unless (verifier-state-error? (verifier-state $verifier))
      (verifier-fail $verifier)))

  (define (verifier-inc-pc $verifier)
    (verifier-pc-set! $verifier (verifier-pc $verifier)))

  (define (verifier-fail $verifier)
    (verifier-state-set! $verifier (make-verifier-state-error))
    (throw error))
)
