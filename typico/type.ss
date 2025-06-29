(library (typico type)
  (export
    type?

    (rename (make-primitive-type primitive-type))
    primitive-type?
    primitive-type-gensym
    primitive-type-datum
    primitive-type-value-predicate?
    primitive-type-value-datum-proc?

    function-type
    function-type?
    function-type-param-types
    function-type-result-type

    forall-type
    forall-type?
    forall-type-arity
    forall-type-type

    variable-type
    variable-type?
    variable-type-index

    application-type
    application-type?
    application-type-type
    application-type-args

    expander-type
    expander-type?
    expander-type-expander

    syntax-type
    syntax-type?
    syntax-type-expander
    syntax-type-datum

    type->datum
    type=?
    type-value-predicate?
    type-value-datum-proc?
    gentype
    define-type)
  (import (micascheme))

  (data (primitive-type gensym datum value-predicate? value-datum-proc?))
  (data (function-type param-types result-type))
  (data (forall-type arity type))
  (data (variable-type index))
  (data (application-type type args))
  (data (expander-type expander))
  (data (syntax-type expander datum))

  (define type?
    (or?
      primitive-type?
      function-type?
      forall-type?
      variable-type?
      application-type?
      expander-type?
      syntax-type?))

  (define make-primitive-type
    (case-lambda
      (($gensym $datum)
        (make-primitive-type $gensym $datum #f))
      (($gensym $datum $value-predicate?)
        (make-primitive-type $gensym $datum $value-predicate? #f))
      (($gensym $datum $value-predicate? $value-datum-proc?)
        (primitive-type $gensym $datum $value-predicate? $value-datum-proc?))))

  (define-rule-syntax (gentype datum arg ...)
    (make-primitive-type (gensym) 'datum arg ...))

  (define-case-syntax (define-type id arg ...)
    #`(define #,(identifier-append #'id #'id #'- #'type)
      (gentype id arg ...)))

  (define (type->datum $type)
    (switch-exhaustive $type
      ((primitive-type? $primitive-type)
        (primitive-type-datum $primitive-type))
      ((function-type? $function-type)
        `(function
          (
          ,@(map*
            type->datum
            (lambda ($type) `(,(type->datum $type) ...))
            (function-type-param-types $function-type)))
          ,(type->datum (function-type-result-type $function-type))))
      ((forall-type? $forall-type)
        `(forall
          ,(forall-type-arity $forall-type)
          ,(type->datum (forall-type-type $forall-type))))
      ((variable-type? $variable-type)
        `(variable ,(variable-type-index $variable-type)))
      ((application-type? $application-type)
        `(
          ,(type->datum (application-type-type $application-type))
          ,@(map type->datum (application-type-args $application-type))))
      ((expander-type? $expander-type)
        `(expander ,(expander-type-expander $expander-type)))
      ((syntax-type? $syntax-type)
        `(syntax
          ,(syntax-type-expander $syntax-type)
          ,(syntax-type-datum $syntax-type)))))

  (define (type->syntax $type)
    (switch-exhaustive $type
      ((primitive-type? $primitive-type)
        (primitive-type-datum $primitive-type))
      ((function-type? $function-type)
        `(function
          (
          ,@(map*
            type->datum
            (lambda ($type) `(,(type->datum $type) ...))
            (function-type-param-types $function-type)))
          ,(type->datum (function-type-result-type $function-type))))
      ((forall-type? $forall-type)
        `(forall
          ,(forall-type-arity $forall-type)
          ,(type->datum (forall-type-type $forall-type))))
      ((variable-type? $variable-type)
        `(variable ,(variable-type-index $variable-type)))
      ((application-type? $application-type)
        `(
          ,(type->datum (application-type-type $application-type))
          ,@(map type->datum (application-type-args $application-type))))
      ((expander-type? $expander-type)
        `(expander ,(expander-type-expander $expander-type)))
      ((syntax-type? $syntax-type)
        `(syntax
          ,(syntax-type-expander $syntax-type)
          ,(syntax-type-datum $syntax-type)))))

  (define (type=? $type-a $type-b)
    (switch? $type-a
      ((primitive-type? $primitive-type-a)
        (switch? $type-b
          ((primitive-type? $primitive-type-b)
            (symbol=?
              (primitive-type-gensym $primitive-type-a)
              (primitive-type-gensym $primitive-type-b)))))
      ((function-type? $function-type-a)
        (switch? $type-b
          ((function-type? $function-type-b)
            (and
              (for-all*
                type=?
                (function-type-param-types $function-type-a)
                (function-type-param-types $function-type-b))
              (type=?
                (function-type-result-type $function-type-a)
                (function-type-result-type $function-type-b))))))
      ((forall-type? $forall-type-a)
        (switch? $type-b
          ((forall-type? $forall-type-b)
            (and
              (=
                (forall-type-arity $forall-type-a)
                (forall-type-arity $forall-type-b))
              (type=?
                (forall-type-type $forall-type-a)
                (forall-type-type $forall-type-b))))))
      ((variable-type? $variable-type-a)
        (switch? $type-b
          ((variable-type? $variable-type-b)
            (=
              (variable-type-index $variable-type-a)
              (variable-type-index $variable-type-b)))))
      ((application-type? $application-type-a)
        (switch? $type-b
          ((application-type? $application-type-b)
            (and
              (type=?
                (application-type-type $application-type-a)
                (application-type-type $application-type-b))
              (for-all*
                type=?
                (application-type-args $application-type-a)
                (application-type-args $application-type-b))))))
      ((expander-type? $expander-type-a)
        (switch? $type-b
          ((expander-type? $expander-type-b)
            (eq?
              (expander-type-expander $expander-type-a)
              (expander-type-expander $expander-type-b)))))
      ((syntax-type? $syntax-type-a)
        (switch? $type-b
          ((syntax-type? $syntax-type-b)
            (and
              (eq?
                (syntax-type-expander $syntax-type-a)
                (syntax-type-expander $syntax-type-b))
              (equal?
                (syntax-type-datum $syntax-type-a)
                (syntax-type-datum $syntax-type-b))))))))

  (define (type-value-predicate? $type)
    (switch? $type
      ((primitive-type? $primitive-type)
        (primitive-type-value-predicate? $primitive-type))))

  (define (type-value-datum-proc? $type)
    (switch? $type
      ((primitive-type? $primitive-type)
        (primitive-type-value-datum-proc? $primitive-type))))
)
