(library (leo2 deduce)
  (export
    deduction
    deduction-bind
    deduction-lets
    deduce)
  (import
    (leo2 base)
    (leo2 term))

  (define-rule-syntax (deduction ($deduced) expr)
    (lambda ($deduced)
      (values expr $deduced)))

  (define (deduction-with $val)
    (switch $val
      ((false? $false)
        (throw deduction-with $false))
      ((else $val)
        (deduction ($deduced)
          (values $val $deduced)))))

  (define (failed-deduction)
    (deduction-with #f))

  (define (deduction-bind $deduction $fn)
    (lambda ($deduced)
      (lets
        ((values $val? $deduced) ($deduction $deduced))
        (switch $val?
          ((false? $false) (values $false $deduced))
          ((else $val) (($fn $val) $deduced))))))

  (define-rules-syntax
    ((deduction-lets deduction) deduction)
    ((deduction-lets (id deduction) x ...)
      (deduction-bind deduction
        (lambda (id) (deduction-lets x ...)))))

  (define deduced-deduction
    (lambda ($deduced)
      (values $deduced $deduced)))

  (define (push-deduction $hole $term)
    (lambda ($deduced)
      (values $term
        (push $deduced (cons $hole $term)))))

  (define (deduced-resolve $deduced $term)
    (switch $term
      ((hole? $term)
        (switch (assoc $term $deduced)
          ((false? _) $term)
          ((else $ass)  (deduced-resolve $deduced (cdr $ass)))))
      ((else $term) $term)))

  (define (deduce $evaluate $env $source $target)
    (lets-recursive deduce
      ($env $env)
      ($source (evaluated-ref ($evaluate $source)))
      ($target (evaluated-ref ($evaluate $target)))
      (cond
        ((eq? $source $target)
          (deduction-with $source))
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
          (failed-deduction)))))
)
