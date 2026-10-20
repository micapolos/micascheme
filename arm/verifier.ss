(library (arm verifier)
  (export
    make-verifier
    verifier?
    verifier-state
    verifier-pc
    verifier-functions

    verifier-state-start?
    verifier-state-function?

    verifier-error?

    verifier-emit-a64
    verifier-emit-va64
    verifier-emit

    a64
    va64
    with-verifier
    check-verifier-error)
  (import
    (scheme)
    (throw)
    (switch)
    (syntax)
    (syntaxes)
    (keyword)
    (check)
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

  (define-keywords a64 va64)

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
    (verifier-pc-set! $verifier (+ (verifier-pc $verifier) 1)))

  (define-rules-syntax
    (keywords a64 va64)
    ((verifier-emit-64 var (a64 op ...))
      (keyword? var)
      (begin (verifier-emit-a64 var op) ...))
    ((verifier-emit-64 var (va64 op ...))
      (keyword? var)
      (begin (verifier-emit-va64 var op) ...)))

  (define-rules-syntax
    (keywords a64 va64)
    ((verifier-emit var block ...)
      (keyword? var)
      (begin (verifier-emit-64 var block) ...)))

  (define-rule-syntax
    (with-verifier var block ... result)
    (let ((var (make-verifier)))
      (verifier-emit var block ...)
      result))

  (define-rule-syntax
    (check-verifier-error block ...)
    (let ((var (make-verifier)))
      (check (raises (verifier-emit var block ...)))))
)
