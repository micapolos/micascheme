(library (leo2 term)
  (export
    nothing nothing?
    anything anything?
    native native? native-ref
    native-application native-application? native-application-procedure native-application-args
    type type? type-depth
    variable variable? variable-symbol
    abstraction abstraction? abstraction-procedure
    signature signature? signature-param signature-procedure
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursion recursion? recursion-procedure
    evaluated evaluated? evaluated-ref
    typed typed? typed-type typed-ref
    symbolic symbolic? symbolic-symbol symbolic-ref
    indexed indexed? indexed-index indexed-ref
    annotated annotated? annotated-annotation annotated-ref

    term? term-switch

    abstraction-apply
    signature-apply
    recursion-apply

    binding binding? binding-ref binding-procedure
    binding-apply

    term-body
    branch-ref
    term-procedure?)
  (import (leo2 base))

  (data nothing)
  (data anything)
  (data (type depth))
  (data (native ref))
  (data (native-application procedure args))
  (data (variable symbol))
  (data (abstraction procedure))
  (data (signature param procedure))
  (data (application lhs rhs))
  (data (branch condition consequent alternate))
  (data (recursion procedure))
  (data (symbolic symbol ref))
  (data (indexed index ref))
  (data (annotated annotation ref))
  (data (evaluated ref))
  (data (typed type ref))

  (union
    (term
      nothing
      anything
      type
      symbol
      indexed
      symbolic
      native
      native-application
      variable
      abstraction
      signature
      application
      branch
      recursion
      annotated
      evaluated
      typed))

  (comment
    (term-switch $term
      ((nothing? $nothing) (todo))
      ((anything? $anything) (todo))
      ((type? $type) (todo))
      ((symbol? $symbol) (todo))
      ((indexed? $indexed) (todo))
      ((symbolic? $symbolic) (todo))
      ((native? $native) (todo))
      ((native-application? $native-application) (todo))
      ((variable? $variable) (todo))
      ((abstraction? $abstraction) (todo))
      ((signature? $signature) (todo))
      ((application? $application) (todo))
      ((branch? $branch) (todo))
      ((recursion? $recursion) (todo))
      ((annotated? $annotated) (todo))
      ((evaluated? $evaluated) (todo))
      ((typed? $typed) (todo))))

  (define (abstraction-apply $abstraction $arg)
    (app (abstraction-procedure $abstraction) $arg))

  (define (signature-apply $signature $arg)
    (app (signature-procedure $signature) $arg))

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

  (define (term-body $term)
    (switch $term
      ((evaluated? $evaluated)
        (term-body (evaluated-ref $evaluated)))
      ((else $other)
        $other)))

  (define (branch-ref $branch $condition)
    (app (if $condition branch-consequent branch-alternate) $branch))

  (define (term-procedure? $term)
    (switch? $term
      ((abstraction? $abstraction)
        (abstraction-procedure $abstraction))
      ((signature? $signature)
        (signature-procedure $signature))
      ((recursion? $recursion)
        (recursion-procedure $recursion))))
)
