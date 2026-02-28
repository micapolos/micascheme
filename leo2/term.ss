(library (leo2 term)
  (export
    hole hole? hole-index
    nothing nothing?
    unknown unknown?
    native native? native-ref
    native-application native-application? native-application-procedure native-application-args
    type type? type-depth
    variable variable? variable-index
    signature signature? signature-param signature-procedure
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursion recursion? recursion-procedure
    evaluated evaluated? evaluated-ref
    typed typed? typed-type typed-ref
    ann ann? ann-type ann-ref
    labeled labeled? labeled-label labeled-ref

    mismatch mismatch? mismatch-expected mismatch-actual
    expected expected? expected-ref
    actual actual? actual-ref

    term? term-switch

    signature-apply
    recursion-apply

    branch-ref)
  (import (leo2 base))

  (data*
    (hole index)
    nothing
    unknown
    (type depth)
    (native ref)
    (native-application procedure args)
    (variable index)
    (signature param procedure)
    (application lhs rhs)
    (branch condition consequent alternate)
    (recursion procedure)
    (labeled label ref)
    (evaluated ref)
    (typed type ref)
    (ann type ref)
    (mismatch expected actual)
    (expected ref)
    (actual ref))

  (union
    (term
      hole
      nothing
      type
      native
      native-application
      variable
      procedure  ; arity 1
      signature
      application
      branch
      recursion
      labeled
      evaluated
      typed))

  (define (term-switch-template $term)
    (term-switch $term
      ((hole? $hole) $hole)
      ((nothing? $nothing) $nothing)
      ((type? $type) $type)
      ((native? $native) $native)
      ((native-application? $native-application) $native-application)
      ((variable? $variable) $variable)
      ((procedure? $procedure) $procedure)
      ((signature? $signature) $signature)
      ((application? $application) $application)
      ((branch? $branch) $branch)
      ((recursion? $recursion) $recursion)
      ((labeled? $labeled) $labeled)
      ((evaluated? $evaluated) $evaluated)
      ((typed? $typed) $typed)))

  (define (signature-apply $signature $arg)
    ((signature-procedure $signature) $arg))

  (define (recursion-apply $recursion $arg)
    ((recursion-procedure $recursion) $arg))

  (define (branch-ref $branch $condition)
    ((if $condition branch-consequent branch-alternate) $branch))
)
