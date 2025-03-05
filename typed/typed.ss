(library (typed typed)
  (export syntax->type)
  (import
    (micascheme)
    (typed type)
    (syntax lookup))

  (data (typed type syntax))

  (define (syntax->type $type-eval $lookup $syntax)
    (syntax-case $syntax (lambda)
      (s
        (string? #'s)
        #'string)
      ((lambda . xs)
        (or
          (syntax-case? #'xs ()
            ((params body)
              (syntax-case? #'params ()
                ((param ...)
                  (lets
                    ($typed-params
                      (map (partial param-syntax->typed $type-eval) (syntaxes param ...)))
                    ($param-types (map typed-type $typed-params))
                    ($param-ids (map typed-syntax $typed-params))
                    (arrow
                      (map typed-type $typed-params)
                      (syntax->type
                        $type-eval
                        (fold-left lookup+typed $lookup $typed-params)
                        #'body)))))))
          (syntax-error $syntax "invalid lambda")))
      ((lhs args ...)
        (switch $lhs-type
          ((arrow? $arrow)
            (lets
              ($params (arrow-params $arrow))
              ($arg-types (map typed-type $args))
              (cond
                ((not (= (length $params) (length $arg-types)))
                  (syntax-error $syntax )
            )
          ((else $type)
            (syntax-error
              (typed-syntax $target)
              "not a function")))))))))

  (define (lookup+typed $lookup $typed)
    (lookup+ $lookup
      (typed-syntax $typed)
      (typed-type $typed)))

  (define (param-syntax->typed $type-eval $syntax)
    (syntax-case $syntax ()
      ((type id)
        (typed ($type-eval #'type) #'id))
      (else
        (syntax-error $syntax "invalid param"))))
)
