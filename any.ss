(library (any)
  (export
    any any? any-value
    any-datum any-datum?
    any-boolean any-boolean?
    any-symbol any-symbol?
    any-char any-char?
    any-string any-string?
    any-number any-number?
    any-syntax any-syntax?
    any-fixnum any-fixnum?
    any-flonum any-flonum?
    any-integer any-integer?
    any-type any-type?
    any-list any-list? any-list-item
    any-any-list any-any-list?
    any-option any-option? any-option-item
    any-any-option any-any-option?
    any-fixnum-between any-fixnum-between? any-fixnum-between-min any-fixnum-between-max
    any-integer-between any-integer-between? any-integer-between-min any-integer-between-max
    (rename
      (any-lambda make-any-lambda)
      (any-lambda-syntax any-lambda))
    any-lambda? any-lambda-params any-lambda-result
    any-any-lambda any-any-lambda?)
  (import (scheme) (data) (syntax))

  (data (any value))
  (data any-datum)
  (data any-boolean)
  (data any-symbol)
  (data any-char)
  (data any-string)
  (data any-number)
  (data any-syntax)
  (data any-fixnum)
  (data any-flonum)
  (data any-integer)
  (data any-type)
  (data (any-option item))
  (data (any-list item))
  (data (any-fixnum-between min max))
  (data (any-integer-between min max))

  (data (any-lambda params result)
    ($rtd
      (record-writer $rtd
        (lambda ($any-lambda $port $wr)
          (define $first-param? #t)
          (display "(any-lambda (" $port)
          (for-each
            (lambda ($param)
              (if $first-param?
                (set! $first-param? #f)
                (display " " $port))
              ($wr $param $port))
            ((record-accessor $rtd 0) $any-lambda))
          (display ") " $port)
          ($wr ((record-accessor $rtd 1) $any-lambda) $port)
          (display ")" $port)))))

  (data any-any-list)
  (data any-any-lambda)
  (data any-any-option)

  (define-rule-syntax (any-lambda-syntax (param ...) result)
    (any-lambda (list param ...) result))
)
