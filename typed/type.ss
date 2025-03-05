(library (typed type)
  (export
    any-boolean
    any-fixnum
    any-flonum
    any-char
    any-string
    any-lambda make-any-lambda any-lambda? any-lambda-params any-lambda-result
    any-list any-list? any-list-component
    type-apply)
  (import (micascheme))

  (data any-boolean)
  (data any-fixnum)
  (data any-flonum)
  (data any-char)
  (data any-string)
  (data (any-list component))

  (define-values (make-any-lambda any-lambda? any-lambda-params any-lambda-result)
    (lets
      ($rtd (make-record-type "any-lambda" '((immutable params) (immutable result))))
      ($constructor (record-constructor $rtd))
      ($predicate (record-predicate $rtd))
      ($params-accessor (record-accessor $rtd 0))
      ($result-accessor (record-accessor $rtd 1))
      (run
        (record-type-equal-procedure $rtd
          (lambda ($arrow-t1 $arrow-t2 $eq)
            (and
              ($eq ($params-accessor $arrow-t1) ($params-accessor $arrow-t2))
              ($eq ($result-accessor $arrow-t1) ($result-accessor $arrow-t2)))))
        ; TODO: Implement hash procedure
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
              ($params-accessor $any-lambda))
            (display ") " $port)
            ($wr ($result-accessor $any-lambda) $port)
            (display ")" $port))))
      (values $constructor $predicate $params-accessor $result-accessor)))

  (define-rule-syntax (any-lambda (param ...) result)
    (make-any-lambda (list param ...) result))

  (define (type-apply $target $args)
    (switch $target
      ((any-lambda? $any-lambda)
        (lets
          ($params (any-lambda-params $any-lambda))
          (run
            (when
              (not (= (length $params) (length $args)))
              (throw invalid-arg-count
                `(actual ,(length $args))
                `(expected ,(length $params))))
            (for-each
              (lambda ($index $param $arg)
                (when
                  (not (equal? $param $arg))
                  (throw invalid-arg
                    `(actual ,$arg)
                    `(expected ,$param)
                    `(index ,$index))))
              (iota (length $params)) $params $args))
          (any-lambda-result $any-lambda)))
      ((else $other)
        (throw invalid-target
          `(actual ,$target)
          `(expected any-lambda)))))
)
