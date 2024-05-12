(library (indico expr)
  (export
    value-type? value-type value-type-arity
    function-type? function-type function-type-out-arity
    expr? expr expr-type expr-syntax
    type-arity expr-arity
    syntax->expr
    list-syntax->expr)
  (import
    (scheme)
    (syntax)
    (data)
    (lets)
    (list)
    (procedure)
    (switch)
    (stack)
    (list-syntax)
    (indico keywords))

  (data (value-type arity))
  (data (function-type out-arity))

  (data (expr type syntax))

  (define (type-arity $type)
    (switch-exclusive $type
      ((value-type? (value-type $arity)) $arity)
      ((function-type? _) 1)))

  (define (expr-arity $expr)
    (type-arity (expr-type $expr)))

  (define (list-syntax->expr $default $locals $syntax)
    (lets
      ($bodies
        (syntax->list $syntax))
      ($body-exprs
        (map (partial syntax->expr $default $locals) $bodies))
      ($body-syntaxes
        (map expr-syntax $body-exprs))
      ($body-tmpss
        (map-with ($body-expr $body-exprs)
          (generate-temporaries (indices (expr-arity $body-expr)))))
      ($results
        (flatten $body-tmpss))
      (expr
        (value-type (length $results))
        #`(let-values
          (
            #,@(map-with
              ($body-syntax $body-syntaxes)
              ($body-tmps $body-tmpss)
              #`((#,@$body-tmps) #,$body-syntax)))
          (values #,@$results)))))

  (define (syntax->expr $default $locals $syntax)
    (syntax-case $syntax (variable block function call)
      ((variable index)
        (expr
          (value-type 1)
          (list-ref $locals (datum index))))
      ((block (arg ...) body ...)
        (lets
          ($args
            (syntax->list #'(arg ...)))
          ($arg-exprs
            (map (partial syntax->expr $default $locals) $args))
          ($arg-syntaxes
            (map expr-syntax $arg-exprs))
          ($arg-tmpss
            (map-with ($arg-expr $arg-exprs)
              (generate-temporaries (indices (expr-arity $arg-expr)))))
          ($locals
            (push-list $locals (flatten $arg-tmpss)))
          ($body-expr
            (list-syntax->expr $default $locals #'(body ...)))
          (expr
            (value-type (expr-arity $body-expr))
            #`(let-values
              (
                #,@(map-with
                  ($arg-syntax $arg-syntaxes)
                  ($arg-tmps $arg-tmpss)
                  #`((#,@$arg-tmps) #,$arg-syntax)))
              #,(expr-syntax $body-expr)))))
      ; Remove this: functions are not expressions.
      ((function arity body ...)
        (lets
          ($tmps
            (generate-temporaries (indices (datum arity))))
          ($body-expr
            (list-syntax->expr $default (push-list $locals $tmps) #'(body ...)))
          (expr
            (function-type (expr-arity $body-expr))
            #`(lambda (#,@$tmps) #,(expr-syntax $body-expr)))))
      ((call fn arg ...)
        (lets
          ($fn-expr (syntax->expr $default $locals #'fn))
          ($args-expr (list-syntax->expr $default $locals #'(arg ...)))
          (expr
            (value-type (function-type-out-arity (expr-type $fn-expr)))
            #`(call-with-values
              (lambda () #,(expr-syntax $args-expr))
              #,(expr-syntax $fn-expr)))))
      ($other
        ($default
          (partial list-syntax->expr $default)
          $locals
          #'$other))))
)
