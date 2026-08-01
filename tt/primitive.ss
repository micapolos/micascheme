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
    primitive->syntax
    primitive-unify
    primitive-subst-apply
    primitive-replace
    primitive-generalize
    append-primitive-holes)
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

  (define (primitive-unify $subst $lhs $rhs)
    (switch $lhs
      ((universe? $lhs)
        (and
          (universe? $rhs)
          $subst))
      ((arrow? $lhs)
        (and
          (arrow? $rhs)
          (?unify primitive-unify
            (fold-left
              (partial ?unify primitive-unify)
              $subst
              (arrow-params $lhs)
              (arrow-params $rhs))
            (arrow-result $lhs)
            (arrow-result $rhs))))
      ((class? $lhs)
        (and
          (class? $rhs)
          (declaration=?
            (class-declaration $lhs)
            (class-declaration $rhs))
          (fold-left
            (partial ?unify primitive-unify)
            $subst
            (class-args $lhs)
            (class-args $rhs))))))

  (define (primitive-subst-apply $subst $primitive)
    (primitive-switch $primitive
      ((universe? $universe)
         $universe)
      ((arrow? $arrow)
        (arrow
          (map (partial subst-apply primitive-subst-apply $subst)
            (arrow-params $arrow))
          (subst-apply primitive-subst-apply $subst
            (arrow-result $arrow))))
      ((class? $class)
        (class
          (class-declaration $class)
          (map (partial subst-apply primitive-subst-apply $subst)
            (class-args $class))))))

  (define (primitive-replace $replaced-hole $replacement-term $primitive)
    (switch $primitive
      ((universe? $universe)
        $universe)
      ((arrow? $arrow)
        (arrow
          (map
            (partial term-replace primitive-replace $replaced-hole $replacement-term)
            (arrow-params $arrow))
          (term-replace primitive-replace $replaced-hole $replacement-term
            (arrow-result $arrow))))
      ((class? $class)
        (class
          (class-declaration $class)
          (map
            (partial term-replace primitive-replace $replaced-hole $replacement-term)
            (class-args $class))))))

  (define (primitive-generalize $hole $term)
    (term-generalize primitive-replace $hole $term))

  (define (append-primitive-holes $depth $holes $primitive)
    (switch $primitive
      ((universe? _) $holes)
      ((arrow? $arrow)
        (append-term-holes append-primitive-holes
          (fold-left
            (partial append-term-holes append-primitive-holes $depth)
            $holes
            (arrow-params $arrow))
          (arrow-result $arrow)))
      ((class? $class)
        (fold-left
          (partial append-term-holes append-primitive-holes $depth)
          $holes
          (class-args $class)))))
)
