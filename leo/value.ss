(library (leo value)
  (export
    boolean! boolean-type boolean-type?
    string! string-type string-type?
    number! number-type number-type?
    named! named named? named-name named-value
    tuple! tuple tuple? tuple-items
    choice! choice choice? choice-items
    function function? function-params function-body
    application application? application-function application-args
    recursion recursion? recursion-body
    typed typed? typed-value typed-type)
  (import (micascheme))

  (data (boolean-type))
  (data (string-type))
  (data (number-type))
  (data (named name value))
  (data (tuple items))
  (data (choice items))
  (data (function params body))
  (data (application function args))
  (data (recursion body))

  (data (typed value type))

  (define boolean! (boolean-type))
  (define string! (string-type))
  (define number! (number-type))

  (define-syntax-rule (named! $name $value)
    (named (quote $name) $value))

  (define-syntax-rule (tuple! $item ...)
    (tuple (list $item ...)))

  (define-syntax-rule (choice! $item ...)
    (choice (list $item ...)))
)
