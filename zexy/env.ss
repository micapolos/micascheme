(library (zexy env)
  (export
    env env? env-stack
    env...
    empty-env
    env-contains?
    env-put
    env-get
    env-eval)
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
    (syntax-case $syntax ()
      (($op $arg ...)
        (apply
          (case (datum $op)
            ((+) +)
            ((-) -)
            ((*) *)
            ((shl) shl)
            ((shr) shr)
            ((and) band)
            ((or) bor)
            ((xor) bxor)
            (else (syntax-error #'$op "unknown operator")))
          (map
            (partial env-eval $env)
            (syntax->list #'($arg ...)))))
      ($op
        (switch (datum $op)
          ((number? $number) $number)
          ((else $other)
            (or
              (env-get $env $other)
              (syntax-error #'$op "undefined")))))
      (else (syntax-error $syntax))))
)
