(library (typed type)
  (export
    type? type type-value type-equal-proc type-syntax-proc
    type-apply
    type->syntax
    type=?)
  (import (micascheme) (any))

  (data (type value equal-proc syntax-proc))

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
      ((any? $any)
        #`(any #,(type->syntax (any-value $any))))
      ((type? $type)
        ((type-syntax-proc $type) (type-value $type)))
      ((any-type? $any-type)
        #'any-type)
      ((any-boolean? $any-boolean)
        #'any-boolean)
      ((any-char? $any-char)
        #'any-char)
      ((any-string? $any-string)
        #'any-string)
      ((any-number? $any-number)
        #'any-number)
      ((any-syntax? $any-syntax)
        #'any-syntax)
      ((any-fixnum? $any-fixnum)
        #'any-fixnum)
      ((any-flonum? $any-flonum)
        #'any-flonum)
      ((any-lambda? $any-lambda)
        (any-lambda->syntax $any-lambda))
      ((any-any-lambda? $any-any-lambda)
        #'any-any-lambda)
      ((any-list? $any-list)
        (any-list->syntax $any-list))
      ((any-fixnum-between? $any-fixnum-between)
        (any-fixnum-between->syntax $any-fixnum-between))))

  (define (type=? $type-a $type-b)
    (switch $type-a
      ((type? $type-a)
        (switch? $type-b
          ((type? $type-b)
            (and
              (eq?
                (type-equal-proc $type-a)
                (type-equal-proc $type-b))
              ((type-equal-proc $type-a)
                (type-value $type-a)
                (type-value $type-b))))))
      ((else $other)
        (equal? $other $type-b))))
)
