(library (leo2 deduce)
  (export
    deduction
    deduction-bind
    deduction-lets
    deduce)
  (import
    (leo2 base)
    (leo2 term))

  (define (deduction $val)
    (lambda ($env)
      (values $val $env)))

  (define (deduction-bind $deduction $fn)
    (lambda ($env)
      (lets
        ((values $val? $env) ($deduction $env))
        (switch $val?
          ((false? $false) (values $false $env))
          ((else $val) (($fn $val) $env))))))

  (define-rules-syntax
    ((deduction-lets deduction) deduction)
    ((deduction-lets (id deduction) x ...)
      (deduction-bind deduction
        (lambda (id) (deduction-lets x ...)))))

  (define (push-deduction $term $deduce)
    (lambda ($env)
      (lets
        ((values $val _) ($deduce (cons $term $env)))
        (values $val $env))))

  (define (deduce $evaluate $source $target)
    (letrec
      ((deduce
        (lambda ($source $target)
          (lets
            ($source ($evaluate $source))
            ($target ($evaluate $target))
            (cond
              ((eq? $source $target)
                (deduction $source))
              ((variable? $source)
                (deduction-lets
                  ($source (lookup-deduction $source))
                  (deduce $source $target)))
              ((and (signature? $source) (signature? $target))
                (deduction-lets
                  (_
                    (deduce
                      (signature-param $target)
                      (signature-param $source)))
                  (push-deduction
                    (signature-param $source)
                    (deduce
                      (signature-apply $source (variable 0))
                      (signature-apply $target (variable 0))))))
              (else
                (deduction #f)))))))
      (deduce $source $target)))

  (define env-deduction
    (lambda ($env)
      (values $env $env)))

  (define (lookup-deduction $variable)
    (deduction-lets
      ($env env-deduction)
      (deduction
        (list-ref $env
          (variable-index $variable)))))
)
