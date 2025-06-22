(library (asm type)
  (export
    primitive-type primitive-type? primitive-type-gensym primitive-type-datum
    function-type function-type? function-type-parameter-types function-type-result-type
    type->syntax
    type->datum
    type=?
    syntax->type
    define-primitive-type
    define-function-type)
  (import (micascheme))

  (data (primitive-type gensym datum))
  (data (function-type parameter-types result-type))

  (define (type->syntax $type)
    (switch-exhaustive $type
      ((primitive-type? $primitive-type)
        #`(primitive-type
          '#,(literal->syntax (primitive-type-gensym $primitive-type))
          '#,(literal->syntax (primitive-type-datum $primitive-type))))
      ((function-type? $function-type)
        #`(function-type
          (list*
            #,@(map**
              type->syntax
              (lambda ($type/null)
                (list (if (null? $type/null) #'(list) (type->syntax $type/null))))
              (function-type-parameter-types $function-type)))
          #,(type->syntax (function-type-result-type $function-type))))))

  (define (type->datum $type)
    (switch-exhaustive $type
      ((primitive-type? $primitive-type)
        (primitive-type-datum $primitive-type))
      ((function-type? $function-type)
        `(function
          (,@(map* type->datum type->datum (function-type-parameter-types $function-type)))
          ,(type->datum (function-type-result-type $function-type))))))

  (define (type=? $lhs $rhs)
    (switch? $lhs
      ((primitive-type? $lhs-primitive-type)
        (switch? $rhs
          ((primitive-type? $rhs-primitive-type)
            (symbol=?
              (primitive-type-gensym $lhs-primitive-type)
              (primitive-type-gensym $rhs-primitive-type)))))
      ((function-type? $lhs-function-type)
        (switch? $rhs
          ((function-type? $rhs-function-type)
            (and
              (for-all* type=?
                (function-type-parameter-types $lhs-function-type)
                (function-type-parameter-types $rhs-function-type))
              (type=?
                (function-type-result-type $lhs-function-type)
                (function-type-result-type $rhs-function-type))))))))

  (define-syntax (define-primitive-type $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        #`(define-syntax id
          (lets
            ($gensym (gensym))
            (make-compile-time-value
              (lambda ($lookup $syntax)
                (syntax-case $syntax ()
                  (x
                    (identifier? #'x)
                    (primitive-type $gensym 'id))))))))))

  (define-syntax (define-function-type $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        #`(define-syntax id
          (make-compile-time-value
            (lambda ($lookup $syntax)
              (syntax-case $syntax ()
                ((_ params result)
                  (function-type
                    (map*
                      (partial syntax->type $lookup)
                      (partial syntax->type $lookup)
                      (syntax->list* #'params))
                    (syntax->type $lookup #'result))))))))))

  (define (syntax->type $lookup $syntax)
    (lets
      ($identifier
        (or
          (syntax-selector $syntax)
          (syntax-error $syntax "invalid type")))
      (app
        (or
          ($lookup $identifier)
          (syntax-error $identifier "undefined type"))
        $lookup $syntax)))
)
