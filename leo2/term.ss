(library (leo2 term)
  (export
    nothing nothing?
    anything anything?
    quoted quoted? quoted-ref
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
    labeled labeled? labeled-label labeled-ref

    term? term-switch

    signature-apply
    recursion-apply

    term-body
    branch-ref)
  (import (leo2 base))

  (data nothing)
  (data anything)
  (data (type depth))
  (data (quoted ref))
  (data (native ref))
  (data (native-application procedure args))
  (data (variable index))
  (data (signature param procedure))
  (data (application lhs rhs))
  (data (branch condition consequent alternate))
  (data (recursion procedure))
  (data (labeled label ref))
  (data (evaluated ref))
  (data (typed type ref))

  (union
    (term
      nothing
      anything
      type
      quoted
      native
      native-application
      variable
      procedure  ; assumed arity 1
      signature
      application
      branch
      recursion
      labeled
      evaluated
      typed))

  (define (term-template $term)
    (term-switch $term
      ((nothing? $nothing) $nothing)
      ((anything? $anything) $anything)
      ((type? $type) $type)
      ((quoted? $quoted) $quoted)
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

  (define (term-body $term)
    (switch $term
      ((evaluated? $evaluated)
        (term-body (evaluated-ref $evaluated)))
      ((else $other)
        $other)))

  (define (branch-ref $branch $condition)
    (
      (if $condition branch-consequent branch-alternate)
      $branch))
)
