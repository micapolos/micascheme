(library (leo2 term)
  (export
    native native? native-ref
    native-application native-application? native-application-target native-application-args
    type type? type-depth
    variable variable? variable-symbol
    abstraction abstraction? abstraction-procedure
    abstraction-type abstraction-type? abstraction-type-param abstraction-type-procedure
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursion recursion? recursion-procedure
    evaluated evaluated? evaluated-ref

    abstraction-apply
    abstraction-type-apply
    recursion-apply

    binding binding? binding-ref binding-procedure
    binding-apply)
  (import (leo2 base))

  (data (native ref))
  (data (native-application target args))
  (data (type depth))
  (data (variable symbol))
  (data (abstraction procedure))
  (data (abstraction-type param procedure))
  (data (application lhs rhs))
  (data (branch condition consequent alternate))
  (data (recursion procedure))
  (data (evaluated ref))

  (define (abstraction-apply $abstraction $arg)
    (app (abstraction-procedure $abstraction) $arg))

  (define (abstraction-type-apply $abstraction-type $arg)
    (app (abstraction-type-procedure $abstraction-type) $arg))

  (define (recursion-apply $recursion $arg)
    (app (recursion-procedure $recursion) $arg))

  (define (binding $term $procedure)
    (application
      (abstraction $procedure)
      $term))

  (define (binding? $term)
    (switch? $term
      ((application? $application)
        (abstraction? (application-lhs $application)))))

  (define (binding-procedure $binding)
    (abstraction-procedure (application-lhs $binding)))

  (define (binding-ref $binding)
    (application-rhs $binding))

  (define (binding-apply $binding $arg)
    (app
      (binding-procedure $binding)
      $arg))
)
