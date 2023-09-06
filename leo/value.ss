(library (leo value)
  (export
    anything! anything anything?
    boolean! any-boolean any-boolean?
    string! any-string any-string?
    number! any-number any-number?
    named! named named? named-name named-value
    tuple! tuple tuple? tuple-items
    choice! choice choice? choice-items
    function! function function? function-params function-body
    application! application application? application-function application-args
    variable variable? variable-type
    recursion recursion? recursion-body
    typed typed? typed-value typed-type)
  (import (micascheme))

  (data (anything))
  (data (any-boolean))
  (data (any-string))
  (data (any-number))
  (data (named name value))
  (data (tuple items))
  (data (choice items))
  (data (function params body))
  (data (application function args))
  (data (variable type))
  (data (recursion body))

  (data (typed value type))

  (define anything! (anything))
  (define boolean! (any-boolean))
  (define string! (any-string))
  (define number! (any-number))

  (define-syntax-rule (named! $name $value)
    (named (quote $name) $value))

  (define-syntax-rule (tuple! $item ...)
    (tuple (list $item ...)))

  (define-syntax-rule (choice! $item ...)
    (choice (list $item ...)))

  (define-syntax-rule (function! ($param ...) $body)
    (function (list $param ...) $body))

  (define-syntax-rule (application! $fn $arg ...)
    (application $fn (list $arg ...)))
)
