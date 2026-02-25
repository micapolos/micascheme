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
    (lambda ($subst)
      (values $val $subst)))

  (define (deduction-bind $deduction $fn)
    (lambda ($subst)
      (lets
        ((values $val? $subst) ($deduction $subst))
        (switch $val?
          ((false? $false) (values $false $subst))
          ((else $val) (($fn $val) $subst))))))

  (define-rules-syntax
    ((deduction-lets deduction) deduction)
    ((deduction-lets (id deduction) x ...)
      (deduction-bind deduction
        (lambda (id) (deduction-lets x ...)))))

  (define (deduce $evaluate $source $target)
    (letrec
      ((deduce
        (lambda ($env $source $target)
          (lets
            ($source (evaluated-ref ($evaluate $source)))
            ($target (evaluated-ref ($evaluate $target)))
            (cond
              ((eq? $source $target)
                (deduction $source))
              ((variable? $source)
                (todo))
              ((and (signature? $source) (signature? $target))
                (deduction-lets
                  (_
                    (deduce
                      $env
                      (signature-param $target)
                      (signature-param $source)))
                  (deduce
                    (push $env (signature-param $target))
                    (signature-apply $source (variable 0))
                    (signature-apply $target (variable 0)))))
              (else
                (deduction #f)))))))
      (deduce '() $source $target)))
)
