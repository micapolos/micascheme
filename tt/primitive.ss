(library (tt primitive)
  (export
    declaration
    declaration?
    declaration-id
    declaration-arity
    declaration-eq-syntax
    declaration-datum-syntax
    generate-declaration
    declaration->syntax

    variable
    variable?
    variable-index

    arrow
    arrow?
    arrow-params
    arrow-param...?
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
    (boolean)
    (option)
    (lets)
    (syntax)
    (tt hoas))

  (data (declaration id arity eq-syntax datum-syntax))

  (data (variable index))
  (data (arrow params param...? result))
  (data (class declaration args))
  (union (primitive variable arrow class))

  (define (generate-declaration $name $arity $eq-syntax $datum-syntax)
    (declaration (gensym $name) $arity $eq-syntax $datum-syntax))

  (define (declaration=? $lhs $rhs)
    (symbol=?
      (declaration-id $lhs)
      (declaration-id $rhs)))

  (define (variable=? $lhs $rhs)
    (=
      (variable-index $lhs)
      (variable-index $rhs)))

  (define (primitive->datum $depth $primitive)
    (primitive-switch $primitive
      ((variable? $variable)
        (index->datum (variable-index $variable)))
      ((arrow? $arrow)
        `(pi
          (
            ,@(map
              (partial term->datum primitive->datum $depth)
              (arrow-params $arrow))
            ,@(switch (arrow-param...? $arrow)
              ((false? _) (list))
              ((else $param) (list (term->datum primitive->datum $depth $param) '...))))
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
      #,(literal->syntax (declaration-arity $declaration))
      #'#,(declaration-eq-syntax $declaration)
      #'#,(declaration-datum-syntax $declaration)))

  (define (primitive->syntax $depth $primitive)
    (primitive-switch $primitive
      ((variable? $variable)
        #`(variable
          #,(literal->syntax (variable-index $variable))))
      ((arrow? $arrow)
        #`(arrow
          (list
            #,@(map
              (partial term->syntax primitive->syntax $depth)
              (arrow-params $arrow)))
          #,(switch (arrow-param...? $arrow)
            ((false? _) #'#f)
            ((else $param) (term->syntax primitive->syntax $depth $param)))
          #,(term->syntax primitive->syntax $depth (arrow-result $arrow))))
      ((class? $class)
        #`(class
          #,(declaration->syntax (class-declaration $class))
          (list #,@(map (partial term->syntax primitive->syntax $depth) (class-args $class)))))))

  (define (primitive=? $depth $lhs $rhs)
    (primitive-switch $lhs
      ((variable? $lhs)
        (and
          (variable $rhs)
          (variable=? $lhs $rhs)))
      ((arrow? $lhs)
        (and
          (arrow? $rhs)
          (for-all* (partial term=? primitive=? $depth)
            (arrow-params $lhs)
            (arrow-params $rhs))
          (switch (arrow-param...? $lhs)
            ((false? _)
              (false? (arrow-param...? $rhs)))
            ((else $lhs)
              (lets?
                ($rhs (arrow-param...? $rhs))
                (term=? primitive=? $depth $lhs $rhs))))
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
      ((variable? $lhs)
        (and
          (variable? $rhs)
          (variable=? $lhs $rhs)))
      ((arrow? $lhs)
        (and
          (arrow? $rhs)
          (=
            (length (arrow-params $lhs))
            (length (arrow-params $rhs)))
          (lets?
            ($subst
              (fold-left?
                (partial term-unify primitive-unify)
                $subst
                (arrow-params $lhs)
                (arrow-params $rhs)))
            ($subst
              (option-fold?
                (partial term-unify primitive-unify)
                $subst
                (arrow-param...? $lhs)
                (arrow-param...? $rhs)))
            (term-unify primitive-unify $subst
              (arrow-result $lhs)
              (arrow-result $rhs)))))
      ((class? $lhs)
        (and
          (class? $rhs)
          (declaration=?
            (class-declaration $lhs)
            (class-declaration $rhs))
          (fold-left?
            (partial term-unify primitive-unify)
            $subst
            (class-args $lhs)
            (class-args $rhs))))))

  (define (primitive-subst-apply $subst $primitive)
    (primitive-switch $primitive
      ((variable? _) $subst)
      ((arrow? $arrow)
        (arrow
          (map
            (partial subst-apply primitive-subst-apply $subst)
            (arrow-params $arrow))
          (option-map
            (partial subst-apply primitive-subst-apply $subst)
            (arrow-param...? $arrow))
          (subst-apply primitive-subst-apply $subst
            (arrow-result $arrow))))
      ((class? $class)
        (class
          (class-declaration $class)
          (map (partial subst-apply primitive-subst-apply $subst)
            (class-args $class))))))

  (define (primitive-replace $replaced-hole $replacement-term $primitive)
    (switch $primitive
      ((variable? $variable) $variable)
      ((arrow? $arrow)
        (arrow
          (map
            (partial term-replace primitive-replace $replaced-hole $replacement-term)
            (arrow-params $arrow))
          (option-map
            (partial term-replace primitive-replace $replaced-hole $replacement-term)
            (arrow-param...? $arrow))
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
      ((variable? _) $holes)
      ((arrow? $arrow)
        (lets
          ($holes
            (fold-left
              (partial append-term-holes append-primitive-holes $depth)
              $holes
              (arrow-params $arrow)))
          ($holes
            (switch (arrow-param...? $arrow)
              ((false? _) $holes)
              ((else $param) (append-term-holes append-primitive-holes $depth $param))))
          (append-term-holes append-primitive-holes $depth $holes
            (arrow-result $arrow))))
      ((class? $class)
        (fold-left
          (partial append-term-holes append-primitive-holes $depth)
          $holes
          (class-args $class)))))
)
