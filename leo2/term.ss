(library (leo2 term)
  (export
    native native? native-ref
    native-application native-application? native-application-procedure native-application-args
    type type? type-depth
    variable variable? variable-symbol
    abstraction abstraction? abstraction-procedure
    abstraction-type abstraction-type? abstraction-type-param abstraction-type-procedure
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursion recursion? recursion-procedure
    evaluated evaluated? evaluated-ref
    typed typed? typed-type typed-ref
    symbolic symbolic? symbolic-symbol symbolic-ref
    indexed indexed? indexed-index indexed-ref

    type-of

    abstraction-apply
    abstraction-type-apply
    recursion-apply

    binding binding? binding-ref binding-procedure
    binding-apply)
  (import (leo2 base))

  ; TODO: Remove abstraction-type as it can be represented as abstraction,
  ; assuming that there'll be param field.

  (data (native ref))
  (data (native-application procedure args))
  (data (type depth))
  (data (variable symbol))
  (data (abstraction procedure))
  (data (abstraction-type param procedure))
  (data (application lhs rhs))
  (data (branch condition consequent alternate))
  (data (recursion procedure))
  (data (evaluated ref))
  (data (typed type ref))
  (data (symbolic symbol ref))
  (data (indexed index ref))
  ; ... and symbol

  (define (type-of $term)
    (switch-exhaustive $term
      ((evaluated? $evaluated)
        (type-of (evaluated-ref $evaluated)))
      ((type? $type)
        (type (+ (type-depth $type) 1)))
      ((typed? $typed)
        (typed-type $typed))))

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
