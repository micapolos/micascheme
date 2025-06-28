(library (typico expander)
  (export
    expander
    id-expander
    predicate-expander
    or-expander
    list->expander
    case-expander
    expand
    expand-value
    check-expand
    check-expand-typed
    check-expand-raises
    datum-expander)
  (import
    (typico base)
    (typico type)
    (typico typed)
    (typico id)
    (typico environment)
    (only (typico expand) type-error))

  (define-rule-syntax (expander ($expander $syntax) body)
    (lambda ($expander $syntax) body))

  (define (id-expander $id $type)
    (expander ($expander $syntax)
      (syntax-case? $syntax ()
        (id
          (and (id? #'id) (id=? #'id $id))
          (typed $type (id->symbol #'id))))))

  (define-rule-syntax (predicate-expander test? type)
    (expander ($expander $syntax)
      (syntax-case? $syntax ()
        (x
          (test? (datum x))
          (typed type (datum x))))))

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
        (else (type-error $syntax $type $expected-type)))))

  (define (typed->test-datum $typed)
    `(
      ,(type->datum (typed-type $typed))
      ,(typed-value $typed)))

  (define-rule-syntax (check-expand expander in out)
    (check
      (equal?
        (typed->test-datum (expand expander (datum/annotation in)))
        'out)))

  (define-rule-syntax (check-expand-typed expander in out)
    (check
      (equal?
        (expand expander (datum/annotation in))
        out)))

  (define-rule-syntax (check-expand-raises expander in)
    (check
      (raises
        (expand expander (datum/annotation in)))))

  (define-case-syntaxes
    ((datum-expander id type proc)
      (id? #'id)
      #'(case-expander id (typed type 'proc)))
    ((datum-expander (id param-type ... vararg-param-type dots) (result-type proc))
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
                (lets
                  ($arg-values (map typed-value $typed-args))
                  ($vararg-values (map typed-value $typed-varargs))
                  ($value-datum-proc? (type-value-datum-proc? $result-type))
                  ($arg-value-predicate?s (map type-value-predicate? $param-types))
                  ($vararg-value-predicate? (type-value-predicate? $vararg-param-type))
                  ($datum `(proc ,@$arg-values ,@$vararg-values))
                  (cond
                    ((and $value-datum-proc?
                      (for-all
                        (lambda ($arg-value-predicate? $arg-value)
                          (and $arg-value-predicate? ($arg-value-predicate? $arg-value)))
                        $arg-value-predicate?s $arg-values)
                      $vararg-value-predicate?
                      (for-all
                        (lambda ($arg-value) ($vararg-value-predicate? $arg-value))
                        $vararg-values))
                      ($value-datum-proc? (eval $datum (typico-environment))))
                    (else $datum)))))))))
    ((datum-expander (id param-type ...) (result-type proc))
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
                (lets
                  ($arg-values (map typed-value $typed-args))
                  ($value-datum-proc? (type-value-datum-proc? $result-type))
                  ($value-predicate?s (map type-value-predicate? $param-types))
                  ($datum `(proc ,@$arg-values))
                  (cond
                    ((and $value-datum-proc?
                      (for-all
                        (lambda ($value-predicate? $arg-value)
                          (and $value-predicate? ($value-predicate? $arg-value)))
                        $value-predicate?s $arg-values))
                      ($value-datum-proc? (eval $datum (typico-environment))))
                    (else $datum))))))))))
)
