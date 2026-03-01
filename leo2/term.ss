(library (leo2 term)
  (export
    primitive?
    hole hole? hole-index
    nothing nothing?
    unknown unknown?
    native-type native-type?
    native native? native-ref
    native-application native-application? native-application-lambda native-application-args
    type type? type-depth
    variable variable? variable-index
    lambda-type lambda-type? lambda-type-param lambda-type-lambda
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursion recursion? recursion-lambda
    evaluated evaluated? evaluated-ref
    typed typed? typed-type typed-ref
    ann ann? ann-type ann-ref
    labeled labeled? labeled-label labeled-ref
    lambda?

    mismatch mismatch? mismatch-expected mismatch-actual
    expected expected? expected-ref
    actual actual? actual-ref

    term? term-switch

    lambda-type-apply
    recursion-apply

    branch-ref)
  (import (leo2 base))

  (define (primitive? $x)
    (or
      (null? $x)
      (boolean? $x)
      (number? $x)
      (char? $x)
      (string? $x)
      (bytevector? $x)))

  (data*
    (hole index)
    nothing
    unknown
    (type depth)
    native-type
    (native ref)
    (native-application lambda args)
    (variable index)
    (lambda-type param lambda)
    (application lhs rhs)
    (branch condition consequent alternate)
    (recursion lambda)
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
      lambda  ; arity 1
      lambda-type
      application
      branch
      recursion
      labeled
      evaluated
      typed))

  (define lambda? procedure?)

  (define (term-switch-template $term)
    (term-switch $term
      ((hole? $hole) $hole)
      ((nothing? $nothing) $nothing)
      ((type? $type) $type)
      ((native? $native) $native)
      ((native-application? $native-application) $native-application)
      ((variable? $variable) $variable)
      ((lambda? $lambda) $lambda)
      ((lambda-type? $lambda-type) $lambda-type)
      ((application? $application) $application)
      ((branch? $branch) $branch)
      ((recursion? $recursion) $recursion)
      ((labeled? $labeled) $labeled)
      ((evaluated? $evaluated) $evaluated)
      ((typed? $typed) $typed)))

  (define (lambda-type-apply $lambda-type $arg)
    ((lambda-type-lambda $lambda-type) $arg))

  (define (recursion-apply $recursion $arg)
    ((recursion-lambda $recursion) $arg))

  (define (branch-ref $branch $condition)
    ((if $condition branch-consequent branch-alternate) $branch))
)
