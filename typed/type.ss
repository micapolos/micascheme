(library (typed type)
  (export
    type-apply
    type->syntax
    type=?)
  (import (micascheme) (any))

  ; (define-values (make-any-lambda any-lambda? any-lambda-params any-lambda-result)
  ;   (lets
  ;     ($rtd (make-record-type "any-lambda" '((immutable params) (immutable result))))
  ;     ($constructor (record-constructor $rtd))
  ;     ($predicate (record-predicate $rtd))
  ;     ($params-accessor (record-accessor $rtd 0))
  ;     ($result-accessor (record-accessor $rtd 1))
  ;     (run
  ;       (record-type-equal-procedure $rtd
  ;         (lambda ($arrow-t1 $arrow-t2 $eq)
  ;           (and
  ;             ($eq ($params-accessor $arrow-t1) ($params-accessor $arrow-t2))
  ;             ($eq ($result-accessor $arrow-t1) ($result-accessor $arrow-t2)))))
  ;       ; TODO: Implement hash procedure
  ;       (record-writer $rtd
  ;         (lambda ($any-lambda $port $wr)
  ;           (define $first-param? #t)
  ;           (display "(any-lambda (" $port)
  ;           (for-each
  ;             (lambda ($param)
  ;               (if $first-param?
  ;                 (set! $first-param? #f)
  ;                 (display " " $port))
  ;               ($wr $param $port))
  ;             ($params-accessor $any-lambda))
  ;           (display ") " $port)
  ;           ($wr ($result-accessor $any-lambda) $port)
  ;           (display ")" $port))))
  ;     (values $constructor $predicate $params-accessor $result-accessor)))

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

  (define (any-lambda->syntax $any-lambda)
    #`(any-lambda
      (#,@(map type->syntax (any-lambda-params $any-lambda)))
      #,(type->syntax (any-lambda-result $any-lambda))))

  (define (any-list->syntax $any-list)
    #`(any-list
      #,(type->syntax (any-list-item $any-list))))

  (define (any-fixnum-between->syntax $any-fixnum-between)
    #`(any-fixnum-between
      #,(datum->syntax #'any-fixnum-between->syntax (any-fixnum-between-min $any-fixnum-between))
      #,(datum->syntax #'any-fixnum-between->syntax (any-fixnum-between-max $any-fixnum-between))))

  (define (type->syntax $type)
    (switch-exhaustive $type
      ((any-type? $any-type)
        #'any-type)
      ((any-boolean? $any-boolean)
        #'any-boolean)
      ((any-char? $any-char)
        #'any-char)
      ((any-string? $any-string)
        #'any-string)
      ((any-fixnum? $any-fixnum)
        #'any-fixnum)
      ((any-flonum? $any-flonum)
        #'any-flonum)
      ((any-lambda? $any-lambda)
        (any-lambda->syntax $any-lambda))
      ((any-list? $any-list)
        (any-list->syntax $any-list))
      ((any-fixnum-between? $any-fixnum-between)
        (any-fixnum-between->syntax $any-fixnum-between))))

  (define (type=? $type-a $type-b)
    (equal? $type-a $type-b))
)
