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

    atomic
    atomic?
    atomic-syntax
    atomic-ref

    class
    class?
    class-declaration
    class-args

    primitive?
    primitive-switch

    primitive=?
    primitive->datum
    primitive->syntax

    literal->atomic
    primitive-apply-term)
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
  (data (atomic syntax ref))
  (data (class declaration args))
  (union (primitive universe arrow atomic class))

  (define (literal->atomic $literal)
    (atomic (literal->syntax $literal) $literal))

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
      ((atomic? $atomic)
        (syntax->datum (atomic-syntax $atomic)))
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
      ((atomic? $atomic)
        (atomic-syntax $atomic))
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
      ((atomic? $lhs)
        (and
          (atomic? $rhs)
          (equal?
            (atomic-ref $lhs)
            (atomic-ref $rhs))))
      ((class? $lhs)
        (and
          (class? $rhs)
          (declaration=?
            (class-declaration $lhs)
            (class-declaration $rhs))
          (for-all* (partial term=? primitive=? $depth)
            (class-args $lhs)
            (class-args $rhs))))))

  (define (primitive-apply-term $fn . $args)
    (cond
      ((for-all atomic? (cons $fn $args))
        (literal->atomic (apply (atomic-ref $fn) (map atomic-ref $args))))
      (else
        (apply application* $fn $args))))
)
