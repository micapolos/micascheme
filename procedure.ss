(library (procedure)
  (export
    run
    run-void
    repeat
    repeat-indexed
    identity
    once-proc
    checking-once
    app
    values-app
    partial
    values-apply
    todo TODO
    dot
    dot-app
    ignore
    combiner
    combiner-2
    ordered-by)
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

  (define-rule-syntax (repeat-indexed ($index $count) $body ...)
    (do
      (($index 0 (add1 $index)))
      ((= $index $count) (void))
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

  (define-syntax dot-app
    (syntax-rules ()
      ((_ $x) $x)
      ((_ $fn $x) ($fn $x))
      ((_ $fn* ... $fn $x) (dot-app $fn* ... ($fn $x)))))

  (define-rule-syntax (dot $fn ...)
    (lambda ($x) (dot-app $fn ... $x)))

  (define (ignore $in $out) $out)

  (define-case-syntax (combiner combine proc arity)
    (let
      (($temporaries (generate-temporaries (iota (datum arity))))
       ($proc #'proc))
      #`(lambda (#,@$temporaries)
        (combine #,@(map
          (lambda ($temporary) #`(#,$proc #,$temporary))
          $temporaries)))))

  (define-rule-syntax (combiner-2 combine proc)
    (combiner combine proc 2))

  (define-rule-syntax (ordered-by order proc ...)
    (combiner-2 order (dot proc ...)))
)
