(library (procedure)
  (export
    run
    run-void
    repeat
    identity
    once-proc
    checking-once
    app
    values-app
    partial
    values-apply
    todo TODO)
  (import
    (scheme)
    (syntax)
    (throw))

  (define-syntax run
    (syntax-rules ()
      ((_)
        (let () (void)))
      ((_ $item ...)
        (let () $item ...))))

  ; TODO: Replace run-void with run.
  (define-syntax run-void
    (syntax-rules ()
      ((_ $item ...)
        (let () $item ... (void)))))

  (define-rule-syntax (repeat $count $body ...)
    (do
      (($i $count (- $i 1)))
      ((zero? $i) (void))
      $body ...))

  (define-rule-syntax (once-proc $proc)
    (let ()
      (define $applied? #f)
      (lambda ()
        (when $applied? (throw once-proc $proc))
        (set! $applied? #t)
        ($proc))))

  (define-rule-syntax (checking-once $body)
    (let ()
      (define $applied? #f)
      (lambda ()
        (when $applied? (error `checking-once "called twice" (quote $body)))
        (set! $applied? #t)
        $body)))

  (define-rule-syntax (app $fn $arg ...)
    ($fn $arg ...))

  (define-syntax (values-app $syntax)
    (syntax-case $syntax ()
      ((_ $fn ($arity $expr) ...)
        (and
          (for-all integer? (datum ($arity ...)))
          (for-all nonnegative? (datum ($arity ...))))
        (let*
          (
            ($arities (map syntax->datum (syntax->list #'($arity ...))))
            ($exprs (syntax->list #'($expr ...)))
            ($tmps (map generate-temporaries (map iota $arities))))
          #`(let-values
              (
                #,@(map
                  (lambda ($tmps $expr) #`((#,@$tmps) #,$expr))
                  $tmps $exprs))
              ($fn #,@(apply append $tmps)))))))

  (define (partial $proc . $partial-args)
    (lambda $args
      (apply $proc (append $partial-args $args))))

  (define (todo)
    (throw todo))

  (define-syntax (TODO $syntax)
    (cond
      ((identifier? $syntax) (syntax (todo)))
      (else (syntax-error $syntax))))

  (define identity (lambda (x) x))

  (define-rule-syntax (values-apply $expr $proc)
    (call-with-values (lambda () $expr) $proc))
)
