(library (tt type)
  (export
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
    type-declaration-name
    type-declaration-arity

    declared-type
    declared-type?
    declared-type-declaration
    declared-type-args

    type-type
    type-type?

    type?

    symbol->type
    type->datum)
  (import
    (scheme)
    (procedure)
    (switch)
    (lets)
    (list)
    (data))

  (data (forall-type arity procedure))
  (data (lambda-type params results))
  (data (type-declaration id name arity))
  (data (declared-type declaration args))
  (data type-type)

  (define (type? $obj)
    (or
      (forall-type? $obj)
      (lambda-type? $obj)
      (declared-type? $obj)
      (type-type? $obj)))

  (define (symbol->type $symbol)
    (declared-type
      (type-declaration (gensym) $symbol 0)
      (list)))

  (define (index->symbol $depth)
    (string->symbol (string-append "t" (number->string (+ $depth 1)))))

  (define (depth-types->datum $depth $types)
    (map (partial depth-type->datum $depth) $types))

  (define (depth-type->datum $depth $type)
    (switch $type
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
          ($name (type-declaration-name (declared-type-declaration $declared-type)))
          ($args (declared-type-args $declared-type))
          (case (length $args)
            ((0) $name)
            (else `(,$name ,@(depth-types->datum $depth $args))))))
      ((type-type? $type-type) 'type)))

  (define (type->datum $type)
    (depth-type->datum 0 $type))
)
