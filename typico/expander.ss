(library (typico expander)
  (export
    expander
    id-expander
    predicate-expander
    or-expander
    list->expander
    case-expander
    expand
    expand?
    expand-value
    expand-value?
    expand-syntax-type?
    expand-function
    check-expand
    check-expand-typed
    check-expand-raises
    datum-expander
    scheme-expander
    macro-expander)
  (import
    (typico base)
    (typico type)
    (typico typed)
    (typico id)
    (typico fragment)
    ; TODO: Get rid of this dependency
    (typico core environment))

  (define-rule-syntax (expander ($expander $syntax) body)
    (lambda ($expander $syntax) body))

  (define (id-expander $id $type)
    (expander ($expander $syntax)
      (syntax-case? $syntax ()
        (id
          (and (id? #'id) (id=? #'id $id))
          (typed $type (id->fragment #'id))))))

  (define-rule-syntax (predicate-expander test? type)
    (expander ($expander $syntax)
      (syntax-case? $syntax ()
        (x
          (test? (datum x))
          (typed type (pure-fragment (datum x)))))))

  (define (or-expander . $expanders)
    (list->expander $expanders))

  (define (list->expander $expanders)
    (expander ($expander $syntax)
      (exists (lambda ($x) ($x $expander $syntax)) $expanders)))

  (define-rules-syntax (literals keywords)
    ((case-expander (keywords keyword ...) (id param ...) ($expander) body)
      (expander ($expander $syntax)
        (syntax-case? $syntax (id keyword ...)
          ((id param ...) body))))
    ((case-expander (keywords keyword ...) (id param ...) body)
      (case-expander (keywords keyword ...) (id param ...) ($expander) body))
    ((case-expander (id param ...) ($expander) body)
      (case-expander (keywords) (id param ...) ($expander) body))
    ((case-expander (id param ...) body)
      (case-expander (id param ...) ($expander) body))
    ((case-expander id ($expander) body)
      (expander ($expander $syntax)
        (syntax-case? $syntax (id)
          (id body))))
    ((case-expander id body)
      (case-expander id ($expander) body)))

  (define (expand? $expander $syntax)
    ($expander $expander $syntax))

  (define (expand $expander $syntax)
    (or
      (expand? $expander $syntax)
      (syntax-error $syntax)))

  (define (expand-value $expander $expected-type $syntax)
    (lets
      ((typed $type $value) (expand $expander $syntax))
      (cond
        ((type=? $type $expected-type) $value)
        (else (syntax-error $syntax "invalid type")))))

  (define (expand-value? $expander $expected-type $syntax)
    (lets
      ($typed (expand $expander $syntax))
      (and
        (type=? (typed-type $typed) $expected-type)
        (typed-value $typed))))

  (define (expand-pure-value? $expander $expected-type $syntax)
    (or
      (expand-value? $expander $expected-type $syntax)
      (switch? $expected-type
        ((application-type? $application-type)
          (switch (application-type-type $application-type)
            ((generic-type? $generic-type)
              (and
                (= (generic-type-arity $generic-type) 1)
                (expand-pure-value? $expander $expected-type
                  (syntax->datum/annotation #`(pure #,$syntax))))))))))

  (define (expand-syntax-type? $expander $syntax)
    (lets
      ((typed $type _) (expand $expander $syntax))
      (and (syntax-type? $type) $type)))

  (define (expand-function $expander $syntax)
    (lets
      ($typed (expand $expander $syntax))
      (switch (typed-type $typed)
        ((function-type? _) $typed)
        ((else $other) (syntax-error $syntax "not a function")))))

  (define (typed->test-datum $typed)
    (lets
      ($fragment (typed-value $typed))
      `(
        ,(type->datum (typed-type $typed))
        (import ,@(fragment-imports $fragment))
        ,(fragment-obj $fragment))))

  (define-rule-syntax (check-expand expander in out)
    (check
      (equal?
        (typed->test-datum (expand expander 'in))
        'out)))

  (define-rule-syntax (check-expand-typed expander in out)
    (check
      (equal?
        (expand expander 'in)
        out)))

  (define-rule-syntax (check-expand-raises expander in)
    (check
      (raises
        (expand expander (datum/annotation in)))))

  (define-case-syntaxes
    ((datum-expander id type fragment)
      (id? #'id)
      #'(case-expander id (typed type fragment)))
    ((datum-expander (id param-type ... vararg-param-type dots result-type) proc-fragment)
      (and (id? #'id) (symbol=? (datum dots) '...))
      (lets
        ($param-temporaries (generate-temporaries #'(param-type ...)))
        ($vararg-temporary (car (generate-temporaries #'(vararg-param-type))))
        #`(case-expander (id #,@$param-temporaries #,$vararg-temporary (... ...)) ($expander)
          (lets
            ($param-types (list param-type ...))
            ($vararg-param-type vararg-param-type)
            ($result-type result-type)
            ($typed-args (map (partial expand $expander) #'(#,@$param-temporaries)))
            ($typed-varargs (map (partial expand $expander) #'(#,$vararg-temporary (... ...))))
            ($arg-types (map typed-type $typed-args))
            ($vararg-types (map typed-type $typed-varargs))
            (and
              (for-all type=? $param-types $arg-types)
              (for-all (partial type=? $vararg-param-type) $vararg-types)
              (typed $result-type
                (fragment-bind-with
                  ($proc proc-fragment)
                  ($arg-values (list->fragment (map typed-value $typed-args)))
                  ($vararg-values (list->fragment (map typed-value $typed-varargs)))
                  (pure-fragment `(,$proc ,@$arg-values ,@$vararg-values)))))))))
    ((datum-expander (id param-type ... result-type) proc-fragment)
      (id? #'id)
      (lets
        ($param-temporaries (generate-temporaries #'(param-type ...)))
        #`(case-expander (id #,@$param-temporaries) ($expander)
          (lets
            ($param-types (list param-type ...))
            ($result-type result-type)
            ($typed-args (map (partial expand $expander) #'(#,@$param-temporaries)))
            ($arg-types (map typed-type $typed-args))
            (and
              (for-all type=? $param-types $arg-types)
              (typed $result-type
                (fragment-bind-with
                  ($proc proc-fragment)
                  ($arg-values (list->fragment (map typed-value $typed-args)))
                  (pure-fragment `(,$proc ,@$arg-values))))))))))

  (define-rule-syntax (scheme-expander id type prim)
    (datum-expander id type (fragment (import (scheme)) prim)))

  (define-case-syntax (macro-expander (keyword ...) pattern body)
    #`(expander ($expander $syntax)
      (lets?
        ($syntax
          (syntax-case? $syntax (keyword ...)
            (pattern #'body)))
        (expand $expander $syntax))))
)
