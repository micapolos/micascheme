(library (typed)
  (export
    typed typed? typed-value typed-type
    native native? native-value native-type
    variable variable? variable-index v0 v1 v2
    function function? function-name function-params function-body function!
    application application? application-fn application-args application!
    tuple tuple? tuple-name tuple-items tuple!
    tuple-ref tuple-ref? tuple-ref-tuple tuple-ref-index)

  (import (micascheme) (variable))

  (data (typed value type))
  (data (native value type))
  (data (function name params body))
  (data (application fn args))
  (data (tuple name items))
  (data (tuple-ref tuple index))

  (define-syntax-rule (function! (name param ...) body)
    (function (quote name) (list param ...) body))

  (define-syntax-rule (application! fn arg ...)
    (application fn (list arg ...)))

  (define-syntax-rule (tuple! (name item ...))
    (tuple (quote name) (list item ...)))
)