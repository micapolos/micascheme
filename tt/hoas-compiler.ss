(library (tt hoas-compiler)
  (export
    universe
    universe?

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

    typed
    typed?
    typed-type
    typed-ref

    compile-type-term
    compile-typed-syntax)
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

  (data (declaration id arity))

  (data universe)
  (data (arrow params results))
  (data (class declaration args))
  (union (type universe arrow class))

  (data (typed type ref))

  (define (declaration=? $lhs $rhs)
    (symbol=?
      (declaration-id $lhs)
      (declaration-id $rhs)))

  (define (type=? $depth $lhs $rhs)
    (type-switch $lhs
      ((universe? $lhs)
        (universe? $rhs))
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

  (define (compile-arrow-results $lookup $syntax)
    (syntax-case $syntax (%values %void)
      ((%values xs ...)
        (map (partial compile-type-term $lookup) #'(xs ...)))
      (%void
        (list))
      (x
        (list (compile-type-term $lookup #'x)))))

  (define (compile-type-term $lookup $syntax)
    (syntax-case $syntax (%type %forall %lambda %...)
      (id
        (and (identifier? #'id) ($lookup #'id))
        (switch ($lookup #'id)
          ((declaration? $declaration)
            (lets
              ($declaration-arity (declaration-arity $declaration))
              (cond
                ((= $declaration-arity 0)
                  (native (class $declaration (list))))
                (else
                  (syntax-error #'id)))))
          ((term? $term)
            $term)
          ((else $other)
            (syntax-error #'id))))
      ((id arg ...)
        (and (identifier? #'id) ($lookup #'id))
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
          ((else $other) (syntax-error #'id))))
      (%type
        (native universe))
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
      ((%lambda param ... vararg-param %... results)
        (native
          (arrow
            (append
              (map (partial compile-type-term $lookup) #'(param ...))
              (compile-type-term $lookup #'vararg-param))
            (compile-arrow-results $lookup #'results))))
      ((%lambda param ... results)
        (native
          (arrow
            (map (partial compile-type-term $lookup) #'(param ...))
            (compile-arrow-results $lookup #'results))))
      (x (syntax-error #'x "not type"))))

  (define (compile-typed-syntax $lookup $syntax)
    (syntax-case $syntax (%type)
      ((%type t)
        (typed
          universe
          (compile-type-term $lookup #'t)))
      (x (syntax-error #'x "not typed"))))
)
