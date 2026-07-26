(library (tt hoas-compiler)
  (export
    arrow
    arrow?
    arrow-params
    arrow-results

    declaration
    declaration?
    declaration-id
    declaration-arity

    class
    class?
    class-declaration
    class-args

    type?
    type-switch

    type=?
    type-term=?

    compile-type-term)
  (import
    (scheme)
    (data)
    (union)
    (lets)
    (list)
    (procedure)
    (switch)
    (tt hoas)
    (tt lookup)
    (prefix (tt keywords) %))

  (data (arrow params results))
  (data (declaration id arity))
  (data (class declaration args))

  (union (type arrow class))

  (define (declaration=? $lhs $rhs)
    (symbol=?
      (declaration-id $lhs)
      (declaration-id $rhs)))

  (define (type=? $depth $lhs $rhs)
    (type-switch $lhs
      ((arrow? $lhs)
        (and
          (arrow? $rhs)
          (for-all*
            (partial type-term=? $depth)
            (arrow-params $lhs)
            (arrow-params $rhs))
          (for-all*
            (partial type-term=? $depth)
            (arrow-results $lhs)
            (arrow-results $rhs))))
      ((class? $lhs)
        (and
          (class? $rhs)
          (declaration=?
            (class-declaration $lhs)
            (class-declaration $rhs))
          (for-all*
            (partial type-term=? $depth)
            (class-args $lhs)
            (class-args $rhs))))))

  (define type-term=? (partial term=? type=?))

  (define (compile-identifier $syntax)
    (switch $syntax
      ((identifier? $identifier) $identifier)
      ((else $other) (syntax-error $other "not identifier"))))

  (define (compile-results $lookup $syntax)
    (syntax-case $syntax (%values %void)
      ((%values xs ...)
        (map (partial compile-type-term $lookup) #'(xs ...)))
      (%void
        (list))
      (x
        (list (compile-type-term $lookup #'x)))))

  (define (compile-type-term $lookup $syntax)
    (syntax-case $syntax (%forall %lambda)
      ((%forall x)
        (compile-type-term $lookup #'x))
      ((%forall id ids ... x)
        (abstraction
          (lambda ($arg)
            (lets
              ($identifier (compile-identifier #'id))
              (compile-type-term
                (lookup-push free-identifier=? $lookup #'id $arg)
                #'(%forall ids ... x))))))
      ((%lambda param ... results)
        (native
          (arrow
            (map (partial compile-type-term $lookup) #'(param ...))
            (compile-results $lookup #'results))))
      (id
        (identifier? #'id)
        (compile-type-term $lookup #'(id)))
      ((id arg ...)
        (identifier? #'id)
        (switch ($lookup #'id)
          ((declaration? $declaration)
            (lets
              ($args #'(arg ...))
              ($args-arity (length $args))
              ($declaration-arity (declaration-arity $declaration))
              (cond
                ((= $declaration-arity $args-arity)
                  (native
                    (class $declaration
                      (map (partial compile-type-term $lookup) $args))))
                (else
                  (syntax-error #'id
                    (format "invalid arity ~a, expected ~a, in"
                      $args-arity $declaration-arity))))))
          ((term? $term) $term)
          ((else $other) (syntax-error #'id))))))
)
