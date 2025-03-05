(library (typed type)
  (export
    string-t
    fx-t
    lambda-t make-lambda-t lambda-t? lambda-params lambda-result)
  (import (micascheme))

  (data string-t)
  (data fx-t)

  (define-values (make-lambda-t lambda-t? lambda-params lambda-result)
    (lets
      ($rtd (make-record-type "lambda-t" '((immutable params) (immutable result))))
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
          (lambda ($lambda-t $port $wr)
            (define $first-param? #t)
            (display "(lambda-t (" $port)
            (for-each
              (lambda ($param)
                (if $first-param?
                  (set! $first-param? #f)
                  (display " " $port))
                ($wr $param $port))
              ($params-accessor $lambda-t))
            (display ") " $port)
            ($wr ($result-accessor $lambda-t) $port)
            (display ")" $port))))
      (values $constructor $predicate $params-accessor $result-accessor)))

  (define-rule-syntax (lambda-t (param ...) result)
    (make-lambda-t (list param ...) result))
)
