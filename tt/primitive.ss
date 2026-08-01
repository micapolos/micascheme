(library (tt primitive)
  (export
    declaration
    declaration?
    declaration-id
    declaration-arity
    generate-declaration

    universe
    universe?

    arrow
    arrow?
    arrow-params
    arrow-result

    class
    class?
    class-declaration
    class-args

    primitive?
    primitive-switch

    primitive=?
    primitive->datum
    primitive->syntax)
  (import
    (scheme)
    (data)
    (union)
    (procedure)
    (throw)
    (switch)
    (list)
    (syntax)
    (tt hoas))

  (data (declaration id arity))

  (data universe)
  (data (arrow params result))
  (data (class declaration args))
  (union (primitive universe arrow class))

  (define (generate-declaration $name $arity)
    (declaration (gensym $name) $arity))

  (define (declaration=? $lhs $rhs)
    (symbol=?
      (declaration-id $lhs)
      (declaration-id $rhs)))

  (define (primitive->datum $depth $primitive)
    (primitive-switch $primitive
      ((universe? $universe)
        'type)
      ((arrow? $arrow)
        `(pi
          ,@(map (partial term->datum primitive->datum $depth) (arrow-params $arrow))
          ,(term->datum primitive->datum $depth (arrow-result $arrow))))
      ((class? $class)
        (switch (class-args $class)
          ((null? _)
            (string->symbol (symbol->string (declaration-id (class-declaration $class)))))
          ((else $args)
            `(
              ,(string->symbol (symbol->string (declaration-id (class-declaration $class))))
              ,@(map (partial term->datum primitive->datum $depth) $args)))))))

  (define (declaration->syntax $declaration)
    #`(declaration
      '#,(literal->syntax (declaration-id $declaration))
      #,(literal->syntax (declaration-arity $declaration))))

  (define (primitive->syntax $depth $primitive)
    (primitive-switch $primitive
      ((universe? $universe)
        #'universe)
      ((arrow? $arrow)
        #`(arrow
          (list #,@(map (partial term->syntax primitive->syntax $depth) (arrow-params $arrow)))
          #,(term->syntax primitive->syntax $depth (arrow-result $arrow))))
      ((class? $class)
        #`(class
          #,(declaration->syntax (class-declaration $class))
          (list #,@(map (partial term->syntax primitive->syntax $depth) (class-args $class)))))))

  (define (primitive=? $depth $lhs $rhs)
    (primitive-switch $lhs
      ((universe? $lhs)
        (universe? $rhs))
      ((arrow? $lhs)
        (and
          (arrow? $rhs)
          (for-all* (partial term=? primitive=? $depth)
            (arrow-params $lhs)
            (arrow-params $rhs))
          (term=? primitive=? $depth
            (arrow-result $lhs)
            (arrow-result $rhs))))
      ((class? $lhs)
        (and
          (class? $rhs)
          (declaration=?
            (class-declaration $lhs)
            (class-declaration $rhs))
          (for-all* (partial term=? primitive=? $depth)
            (class-args $lhs)
            (class-args $rhs))))))
)
