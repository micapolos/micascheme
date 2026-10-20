(library (arm verifier)
  (export
    make-verifier
    verifier?
    verifier-state
    verifier-functions

    verifier-state-start?
    verifier-state-function?

    verifier-error?

    verifier-emit-a64
    verifier-emit-va64)
  (import
    (scheme)
    (throw)
    (switch)
    (arm va64)
    (arm encode))

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

  (define-record-type verifier-error
    (fields))

  (define-record-type unsupported-function-arity (fields))
  (define-record-type unsupported-function-ret (fields))

  (define (verifier-emit-a64 $verifier $op)
    (switch (verifier-state $verifier)
      ((verifier-state-function? $verifier-state-function)
        (cond
          ((nop? $op)
            (verifier-inc-pc $verifier))
          (else
            (raise (make-verifier-error)))))
      ((else _)
        (raise (make-verifier-error)))))

  (define (verifier-emit-va64 $verifier $op)
    (switch (verifier-state $verifier)
      ((verifier-state-start? _)
        (cond
          ((fn? $op)
            (cond
              ((not (zero? (fn-arity $op)))
                (raise (make-unsupported-function-arity)))
              ((not (zero? (fn-ret $op)))
                (raise (make-unsupported-function-ret)))
              (else
                (verifier-state-set! $verifier (make-verifier-state-function)))))
          (else
            (raise (make-verifier-error)))))
      ((else _)
        (raise (make-verifier-error)))))

  (define (verifier-inc-pc $verifier)
    (verifier-pc-set! $verifier (verifier-pc $verifier)))
)
