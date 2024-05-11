(library (indico expr)
  (export
    expr? expr expr-arity expr-syntax
    syntax->expr)
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
  (data (function-type in-arity out-arity))

  (data (expr type syntax))

  (define (type-arity $type)
    (switch-exclusive $type
      ((value-type? (value-type $arity)) $arity)
      ((function-type? _) 1)))

  (define (expr-arity $expr)
    (type-arity (expr-type $expr)))

  (define (body-syntax->expr $locals $syntax)
    (lets
      ($bodies
        (syntax->list $syntax))
      ($body-exprs
        (map (partial syntax->expr $locals) $bodies))
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

  (define (syntax->expr $locals $syntax)
    (syntax-case $syntax (native get block function call)
      ((native body ...)
        (expr
          (value-type (length (datum (body ...))))
          #'(values body ...)))
      ((get index)
        (expr
          (value-type 1)
          (list-ref $locals (datum index))))
      ((block (arg ...) body ...)
        (lets
          ($args
            (syntax->list #'(arg ...)))
          ($arg-exprs
            (map (partial syntax->expr $locals) $args))
          ($arg-syntaxes
            (map expr-syntax $arg-exprs))
          ($arg-tmpss
            (map-with ($arg-expr $arg-exprs)
              (generate-temporaries (indices (expr-arity $arg-expr)))))
          ($locals
            (push-list $locals (flatten $arg-tmpss)))
          ($body-expr
            (body-syntax->expr $locals #'(body ...)))
          (expr
            (value-type (expr-arity $body-expr))
            #`(let-values
              (
                #,@(map-with
                  ($arg-syntax $arg-syntaxes)
                  ($arg-tmps $arg-tmpss)
                  #`((#,@$arg-tmps) #,$arg-syntax)))
              #,(expr-syntax $body-expr)))))
      ((function arity body ...)
        (lets
          ($tmps
            (generate-temporaries (indices (datum arity))))
          ($body-expr
            (body-syntax->expr (push-list $locals $tmps) #'(body ...)))
          (expr
            (function-type (datum arity) (expr-arity $body-expr))
            #`(lambda (#,@$tmps) #,(expr-syntax $body-expr)))))))
)
