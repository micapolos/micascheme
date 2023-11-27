(library (procedure)
  (export
    once-proc
    checking-once)
  (import
    (scheme)
    (syntax)
    (throw))

  (define-syntax-rule (once-proc $proc)
    (let ()
      (define $applied? #f)
      (lambda ()
        (when $applied? (throw once-proc $proc))
        (set! $applied? #t)
        ($proc))))

  (define-syntax-rule (checking-once $body)
    (let ()
      (define $applied? #f)
      (lambda ()
        (when $applied? (error `checking-once "called twice" (quote $body)))
        (set! $applied? #t)
        $body)))
)
