(library (zexy env)
  (export
    env env? env-stack
    env...
    empty-env
    env-contains?
    env-put
    env-get
    env-eval
    env-reduce)
  (import
    (micascheme)
    (zexy math))

  (data (env stack))

  (define-syntax-rule (env... ($label $value) ...)
    (env (stack (cons (quote $label) $value) ...)))

  (define (empty-env)
    (env (stack)))

  (define (env-put $env $label $value)
    (env
      (push
        (env-stack $env)
        (cons $label $value))))

  (define (env-contains? $env $label)
    (not-false? (assq $label (env-stack $env))))

  (define (env-get $env $label)
    (lets
      ($ass (assq $label (env-stack $env)))
      (and $ass (env-eval $env (cdr $ass)))))

  (define (env-eval $env $syntax)
    (switch (env-reduce $env $syntax)
      ((number? $number) $number)
      ((else _) (syntax-error $syntax))))

  (define (env-reduce $env $syntax)
    (syntax-case $syntax ()
      (($op $arg ...)
        (lets
          ($proc
            (case (datum $op)
              ((+) +)
              ((-) -)
              ((*) *)
              ((shl) shl)
              ((shr) shr)
              ((and) band)
              ((or) bor)
              ((xor) bxor)
              (else #f)))
          ($reduced
            (map
              (partial env-reduce $env)
              (syntax->list #'($arg ...))))
          (or
            (and $proc (for-all number? $reduced)
              (apply $proc $reduced))
            #`(#,#'$op #,@$reduced))))
      ($op
        (switch (datum $op)
          ((number? $number) $number)
          ((symbol? $symbol)
            (or (env-get $env $symbol) #'$op))
          ((else $other) $other)))))
)
