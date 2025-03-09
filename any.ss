(library (any)
  (export
    any-boolean any-boolean?
    any-char any-char?
    any-string any-string?
    any-fixnum any-fixnum?
    any-flonum any-flonum?
    any-integer any-integer?
    any-type any-type?
    any-list any-list? any-list-item
    any-fixnum-between any-fixnum-between? any-fixnum-between-min any-fixnum-between-max
    (rename
      (any-lambda make-any-lambda)
      (any-lambda-syntax any-lambda))
    any-lambda? any-lambda-params any-lambda-result)
  (import (scheme) (data) (syntax))

  (data any-boolean)
  (data any-char)
  (data any-string)
  (data any-fixnum)
  (data any-flonum)
  (data any-integer)
  (data any-type)
  (data (any-list item))
  (data (any-fixnum-between min max))

  (data (any-lambda params result))
  (define-rule-syntax (any-lambda-syntax (param ...) result)
    (any-lambda (list param ...) result))
)
