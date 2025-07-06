(library (typico type)
  (export
    type?

    (rename (make-primitive-type primitive-type))
    primitive-type?
    primitive-type-gensym
    primitive-type-datum

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

    generic-type
    generic-type?
    generic-type-gensym
    generic-type-arity
    generic-type-datum

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

    pair-type
    pair-type?
    pair-type-car
    pair-type-cdr

    literal-type
    literal-type?
    literal-type-literal

    depth-type->datum
    type->datum
    type=?
    gentype
    define-type)
  (import (micascheme))

  (data (primitive-type gensym datum))
  (data (function-type param-types result-type))
  (data (forall-type arity type))
  (data (variable-type index))
  (data (generic-type gensym arity datum))
  (data (application-type type args))
  (data (expander-type expander))
  (data (syntax-type expander datum))
  (data (pair-type car cdr))
  (data (literal-type literal))

  (define type?
    (or?
      primitive-type?
      function-type?
      forall-type?
      variable-type?
      generic-type?
      application-type?
      expander-type?
      syntax-type?
      pair-type?
      pair?
      symbol?))

  (define make-primitive-type
    (case-lambda
      (($gensym $datum)
        (primitive-type $gensym $datum))))

  (define-rule-syntax (gentype datum)
    (make-primitive-type (gensym) 'datum))

  (define-syntax (define-type $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        #`(define #,(identifier-append #'id #'id #'- #'type)
          (gentype id)))
      ((_ (id param ...))
        (for-all identifier? #'(id param ...))
        #`(begin
          (define #,(identifier-append #'id #'generic- #'id #'- #'type)
            (generic-type
              (gensym)
              #,(literal->syntax (length #'(param ...)))
              'id))
          (define (#,(identifier-append #'id #'id #'- #'type) param ...)
            (application-type
              #,(identifier-append #'id #'generic- #'id #'- #'type)
              (list param ...)))))))

  (define (type->datum $type)
    (depth-type->datum 0 $type))

  (define (depth-type->datum $depth $type)
    (switch $type
      ((primitive-type? $primitive-type)
        (primitive-type-datum $primitive-type))
      ((function-type? $function-type)
        `(->
          ,@(map*
            (partial depth-type->datum $depth)
            (lambda ($type) `(,(depth-type->datum $depth $type) ...))
            (function-type-param-types $function-type))
          ,(depth-type->datum $depth (function-type-result-type $function-type))))
      ((forall-type? $forall-type)
        (lets
          ($arity (forall-type-arity $forall-type))
          ($depth (+ $depth $arity))
          `(forall
            ,@(map (partial depth-type->datum $depth) (map variable-type (reverse (iota $arity))))
            ,(depth-type->datum $depth (forall-type-type $forall-type)))))
      ((variable-type? $variable-type)
        (string->symbol
          (string-append "t"
            (number->string
              (- $depth (variable-type-index $variable-type))))))
      ((generic-type? $generic-type)
        `(generic
          ,(generic-type-arity $generic-type)
          ,(generic-type-datum $generic-type)))
      ((application-type? $application-type)
        `(
          ,(generic-type-datum (application-type-type $application-type))
          ,@(map
            (partial depth-type->datum $depth)
            (application-type-args $application-type))))
      ((expander-type? $expander-type)
        `(expander ,(expander-type-expander $expander-type)))
      ((syntax-type? $syntax-type)
        `(syntax
          ,(syntax-type-expander $syntax-type)
          ,(syntax-type-datum $syntax-type)))
      ((pair-type? $pair-type)
        `(
          ,(depth-type->datum $depth (pair-type-car $pair-type))
          ,(depth-type->datum $depth (pair-type-cdr $pair-type))))
      ((pair? $pair)
        `(
          ,(depth-type->datum $depth (car $pair))
          ,(depth-type->datum $depth (cdr $pair))))
      ((literal-type? $literal-type)
        (literal-type-literal $literal-type))
      ((else $other) $other)))

  (define (type=? $type-a $type-b)
    (switch $type-a
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
      ((generic-type? $generic-type-a)
        (switch? $type-b
          ((generic-type? $generic-type-b)
            (symbol=?
              (generic-type-gensym $generic-type-a)
              (generic-type-gensym $generic-type-b)))))
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
                (syntax-type-datum $syntax-type-b))))))
      ((pair-type? $pair-type-a)
        (switch? $type-b
          ((pair-type? $pair-type-b)
            (and
              (type=?
                (pair-type-car $pair-type-a)
                (pair-type-car $pair-type-b))
              (type=?
                (pair-type-cdr $pair-type-a)
                (pair-type-cdr $pair-type-b))))))
      ((pair? $pair-a)
        (switch? $type-b
          ((pair? $pair-b)
            (and
              (type=?
                (car $pair-a)
                (car $pair-b))
              (type=?
                (cdr $pair-a)
                (cdr $pair-b))))))
      ((literal-type? $literal-type-a)
        (switch? $type-b
          ((literal-type? $literal-type-b)
            (equal?
              (literal-type-literal $literal-type-a)
              (literal-type-literal $literal-type-b)))))
      ((else $other-a)
        (equal? $other-a $type-b))))
)
