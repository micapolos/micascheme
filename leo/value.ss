(library (leo value)
  (export
    anything! anything anything?
    type! any-type any-type?
    boolean! any-boolean any-boolean?
    string! any-string any-string?
    number! any-number any-number?
    typeof typeof? typeof-value
    named! named named? named-name named-value
    named-get named-get? named-get-named
    tuple! tuple tuple? tuple-items
    tuple-get tuple-get? tuple-get-tuple tuple-get-type
    choice! choice choice? choice-items
    choice-switch! choice-switch choice-switch? choice-switch-choice choice-switch-cases
    function! function function? function-params function-body
    application! application application? application-function application-args
    variable variable? variable-type
    recursion recursion? recursion-body
    typed typed? typed-value typed-type

    typed-values)
  (import (micascheme))

  (data (anything))
  (data (any-type))
  (data (any-boolean))
  (data (any-string))
  (data (any-number))
  (data (typeof value))
  (data (named name value))
  (data (named-get named))
  (data (tuple items))
  (data (tuple-get tuple type))
  (data (choice items))
  (data (choice-switch choice cases))
  (data (function params body))
  (data (application function args))
  (data (variable type))
  (data (recursion body))

  (data (typed value type))

  (define anything! (anything))
  (define type! (any-type))
  (define boolean! (any-boolean))
  (define string! (any-string))
  (define number! (any-number))

  (define-syntax-rule (named! $name $value)
    (named (quote $name) $value))

  (define-syntax-rule (tuple! $item ...)
    (tuple (list $item ...)))

  (define-syntax-rule (choice! $item ...)
    (choice (list $item ...)))

  (define-syntax-rule (choice-switch! $choice $case ...)
    (choice-switch $choice (list $case ...)))

  (define-syntax-rule (function! ($param ...) $body)
    (function (list $param ...) $body))

  (define-syntax-rule (application! $fn $arg ...)
    (application $fn (list $arg ...)))

  (define (typed-values $typed)
    (values (typed-value $typed) (typed-type $typed)))
)
