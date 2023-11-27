(library (procedure)
  (export
    once-proc
    checking-once
    app
    values-app)
  (import
    (scheme)
    (lets)
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

  (define-syntax-rule (app $fn $arg ...)
    ($fn $arg ...))

  (define-syntax values-app
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($arity $expr) ...)
          (and
            (for-all integer? (datum ($arity ...)))
            (for-all nonnegative? (datum ($arity ...))))
          (lets
            ($arities (map syntax->datum (syntax->list #'($arity ...))))
            ($exprs (syntax->list #'($expr ...)))
            ($tmps (map generate-temporaries (map iota $arities)))
            #`(let-values
                (
                  #,@(map
                    (lambda ($tmps $expr) #`((#,@$tmps) #,$expr))
                    $tmps $exprs))
                (#,@(apply append $tmps))))))))
)
