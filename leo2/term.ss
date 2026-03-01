(library (leo2 term)
  (export
    primitive?
    hole hole? hole-index
    nothing nothing?
    unknown unknown?
    native-type native-type?
    native native? native-ref
    native-application native-application? native-application-procedure native-application-args
    type type? type-depth
    variable variable? variable-index
    procedure-type procedure-type? procedure-type-param procedure-type-procedure
    application application? application-lhs application-rhs
    branch branch? branch-condition branch-consequent branch-alternate
    recursion recursion? recursion-procedure
    evaluated evaluated? evaluated-ref
    typed typed? typed-type typed-ref
    ann ann? ann-type ann-ref
    labeled labeled? labeled-label labeled-ref
    procedure

    mismatch mismatch? mismatch-expected mismatch-actual
    expected expected? expected-ref
    actual actual? actual-ref

    term? term-switch

    procedure-type-apply
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
    (native-application procedure args)
    (variable index)
    (procedure-type param procedure)
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
      procedure-type
      application
      branch
      recursion
      labeled
      evaluated
      typed))

  (define-rule-syntax (procedure (id) body)
    (lambda (id) body))

  (define (term-switch-template $term)
    (term-switch $term
      ((hole? $hole) $hole)
      ((nothing? $nothing) $nothing)
      ((type? $type) $type)
      ((native? $native) $native)
      ((native-application? $native-application) $native-application)
      ((variable? $variable) $variable)
      ((procedure? $procedure) $procedure)
      ((procedure-type? $procedure-type) $procedure-type)
      ((application? $application) $application)
      ((branch? $branch) $branch)
      ((recursion? $recursion) $recursion)
      ((labeled? $labeled) $labeled)
      ((evaluated? $evaluated) $evaluated)
      ((typed? $typed) $typed)))

  (define (procedure-type-apply $procedure-type $arg)
    ((procedure-type-procedure $procedure-type) $arg))

  (define (recursion-apply $recursion $arg)
    ((recursion-procedure $recursion) $arg))

  (define (branch-ref $branch $condition)
    ((if $condition branch-consequent branch-alternate) $branch))
)
