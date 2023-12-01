(library (procedure)
  (export
    run
    identity
    once-proc
    checking-once
    app
    values-app
    partial
    todo TODO)
  (import
    (scheme)
    (syntax)
    (throw))

  (define-syntax-rule (run $item ...)
    (let () $item ... (void)))

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

  (define-syntax-rule (app $fn $arg ...)
    ($fn $arg ...))

  (define-syntax values-app
    (lambda ($syntax)
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
                ($fn #,@(apply append $tmps))))))))

  (define (partial $proc . $partial-args)
    (lambda $args
      (apply $proc (append $partial-args $args))))

  (define (todo)
    (throw todo))

  (define-syntax TODO
    (lambda ($syntax)
      (cond
        ((identifier? $syntax) (syntax (todo)))
        (else (syntax-error $syntax)))))

  (define identity (lambda (x) x))
)
