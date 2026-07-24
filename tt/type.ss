(library (tt type)
  (export
    hole-type
    hole-type?
    hole-type-id

    forall-type
    forall-type?
    forall-type-arity
    forall-type-procedure

    lambda-type
    lambda-type?
    lambda-type-params
    lambda-type-results

    type-declaration
    type-declaration?
    type-declaration-id
    type-declaration-arity

    declared-type
    declared-type?
    declared-type-declaration
    declared-type-args

    type-type
    type-type?

    type?

    symbol->type
    type->datum
    type=?

    resolve-hole-type)
  (import
    (scheme)
    (procedure)
    (switch)
    (lets)
    (list)
    (boolean)
    (data))

  (data (type-declaration id arity))

  (data (hole-type id))
  (data (forall-type arity procedure))
  (data (lambda-type params results))
  (data (declared-type declaration args))
  (data type-type)

  (define (type? $obj)
    (or
      (hole-type? $obj)
      (forall-type? $obj)
      (lambda-type? $obj)
      (declared-type? $obj)
      (type-type? $obj)))

  (define (symbol->type $symbol . $args)
    (declared-type
      (type-declaration $symbol (length $args))
      $args))

  (define (index->symbol $depth)
    (string->symbol (string-append "t" (number->string (+ $depth 1)))))

  (define (depth-types->datum $depth $types)
    (map (partial depth-type->datum $depth) $types))

  (define (depth-type->datum $depth $type)
    (switch $type
      ((hole-type? $hole-type)
        `(hole ,(hole-type-id $hole-type)))
      ((forall-type? $forall-type)
        (lets
          ($arity (forall-type-arity $forall-type))
          ($symbols
            (map
              (lambda ($index) (index->symbol (+ $depth $index)))
              (iota $arity)))
          `(forall ,@$symbols
            ,(depth-type->datum
              (+ $depth $arity)
              (apply (forall-type-procedure $forall-type) (map symbol->type $symbols))))))
      ((lambda-type? $lambda-type)
        `(lambda
          ,@(map (partial depth-type->datum $depth) (lambda-type-params $lambda-type))
          ,(case (length (lambda-type-results $lambda-type))
            ((0) 'void)
            ((1) (depth-type->datum $depth (car (lambda-type-results $lambda-type))))
            (else
              `(values
                ,@(depth-types->datum $depth (lambda-type-results $lambda-type)))))))
      ((declared-type? $declared-type)
        (lets
          ($name (type-declaration-id (declared-type-declaration $declared-type)))
          ($args (declared-type-args $declared-type))
          (case (length $args)
            ((0) $name)
            (else `(,$name ,@(depth-types->datum $depth $args))))))
      ((type-type? $type-type) 'type)))

  (define (type->datum $type)
    (depth-type->datum 0 $type))

  (define (resolve-hole-type $type $subst)
    (switch $type
      ((hole-type? $hole-type)
        (switch (assq (hole-type-id $hole-type) $subst)
          ((pair? $pair) (resolve-hole-type (cdr $pair) $subst))
          ((else _) $type)))
      ((else $other) $other)))

  (define (type=? $lhs $rhs)
    (switch-exhaustive $lhs
      ((hole-type? $lhs)
        (switch? $rhs
          ((hole-type? $rhs)
            (symbol=?
              (hole-type-id $lhs)
              (hole-type-id $rhs)))))
      ((forall-type? $lhs)
        (switch? $rhs
          ((forall-type? $rhs)
            (lets
              ($arity (forall-type-arity $lhs))
              (and
                (= $arity (forall-type-arity $rhs))
                (lets
                  ($args (map (lambda (_) (hole-type (gensym))) (iota $arity)))
                  (type=?
                    (apply (forall-type-procedure $lhs) $args)
                    (apply (forall-type-procedure $rhs) $args))))))))
      ((lambda-type? $lhs)
        (switch? $rhs
          ((lambda-type? $rhs)
            (and
              (for-all* type=?
                (lambda-type-params $lhs)
                (lambda-type-params $rhs))
              (for-all* type=?
                (lambda-type-results $lhs)
                (lambda-type-results $rhs))))))
      ((declared-type? $lhs)
        (switch? $rhs
          ((declared-type? $rhs)
            (and
              (equal?
                (declared-type-declaration $lhs)
                (declared-type-declaration $rhs))
              (for-all* type=?
                (declared-type-args $lhs)
                (declared-type-args $rhs))))))
      ((type-type? $lhs)
        (type-type? $rhs))))
)
