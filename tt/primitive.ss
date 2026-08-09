(library (tt primitive)
  (export
    declaration
    declaration?
    declaration-id
    declaration-arity
    declaration-dynamic?
    generate-declaration
    declaration->syntax

    arrow
    arrow?
    arrow-params
    arrow-param...?
    arrow-result

    class
    class?
    class-declaration
    class-args

    tuple
    tuple?
    tuple-args

    choice
    choice?
    choice-args

    primitive?
    primitive-switch

    primitive=?
    primitive->datum
    primitive->syntax
    primitive-dynamic?
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
    (except (list) product)
    (boolean)
    (option)
    (lets)
    (syntax)
    (tt hoas))

  (data (declaration id arity))

  (data (arrow params param...? result))
  (data (class declaration args))
  (data (tuple args))
  (data (choice args))
  (union (primitive number arrow class tuple choice))

  ; make it a field
  (define (declaration-dynamic? _) #t)

  (define (generate-declaration $name $arity)
    (declaration (gensym $name) $arity))

  (define (declaration=? $lhs $rhs)
    (symbol=?
      (declaration-id $lhs)
      (declaration-id $rhs)))

  (define (primitive->datum $depth $primitive)
    (primitive-switch $primitive
      ((number? $number) $number)
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
              ,@(map (partial term->datum primitive->datum $depth) $args)))))
      ((tuple? $tuple)
        `(tuple
          ,@(map (partial term->datum primitive->datum $depth) (tuple-args $tuple))))
      ((choice? $choice)
        `(choice
          ,@(map (partial term->datum primitive->datum $depth) (choice-args $choice))))))

  (define (declaration->syntax $declaration)
    #`(declaration
      '#,(literal->syntax (declaration-id $declaration))
      #,(literal->syntax (declaration-arity $declaration))))

  (define (primitive->syntax $depth $primitive)
    (primitive-switch $primitive
      ((number? $number)
        (literal->syntax $number))
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
          (list #,@(map (partial term->syntax primitive->syntax $depth) (class-args $class)))))
      ((tuple? $tuple)
        #`(tuple
          (list #,@(map (partial term->syntax primitive->syntax $depth) (tuple-args $tuple)))))
      ((choice? $choice)
        #`(choice
          (list #,@(map (partial term->syntax primitive->syntax $depth) (choice-args $choice)))))))

  (define (primitive-dynamic? $depth $primitive)
    (lets
      ($term-dynamic? (partial term-dynamic? primitive-dynamic? $depth))
      (primitive-switch $primitive
        ((number? _) #f)
        ((arrow? $arrow)
          (or
            (exists $term-dynamic? (arrow-params $arrow))
            (option-map $term-dynamic? (arrow-param...? $arrow))
            ($term-dynamic? (arrow-result $arrow))))
        ((class? $class)
          (or
            (declaration-dynamic? (class-declaration $class))
            (exists $term-dynamic? (class-args $class))))
        ((tuple? $tuple)
          (and
            (not (zero? (length (tuple-args $tuple))))
            (exists $term-dynamic? (tuple-args $tuple))))
        ((choice? $choice)
          (or
            (> (length (choice-args $choice)) 1)
            (exists $term-dynamic? (choice-args $choice)))))))

  (define (primitive=? $depth $lhs $rhs)
    (primitive-switch $lhs
      ((number? $lhs)
        (and
          (number? $rhs)
          (= $lhs $rhs)))
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
            (class-args $rhs))))
      ((tuple? $lhs)
        (and
          (tuple? $rhs)
          (for-all* (partial term=? primitive=? $depth)
            (tuple-args $lhs)
            (tuple-args $rhs))))
      ((choice? $lhs)
        (and
          (choice? $rhs)
          (for-all* (partial term=? primitive=? $depth)
            (choice-args $lhs)
            (choice-args $rhs))))))

  (define (primitive-unify $subst $lhs $rhs)
    (switch $lhs
      ((number? $lhs)
        (and
          (number? $rhs)
          (= $lhs $rhs)
          $subst))
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
            (class-args $rhs))))
      ((tuple? $lhs)
        (and
          (tuple? $rhs)
          (fold-left?
            (partial term-unify primitive-unify)
            $subst
            (tuple-args $lhs)
            (tuple-args $rhs))))
      ((choice? $lhs)
        (and
          (choice? $rhs)
          (fold-left?
            (partial term-unify primitive-unify)
            $subst
            (choice-args $lhs)
            (choice-args $rhs))))))

  (define (primitive-subst-apply $subst $primitive)
    (primitive-switch $primitive
      ((number? $number) $number)
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
            (class-args $class))))
      ((tuple? $tuple)
        (tuple
          (map (partial subst-apply primitive-subst-apply $subst)
            (tuple-args $tuple))))
      ((choice? $choice)
        (choice
          (map (partial subst-apply primitive-subst-apply $subst)
            (choice-args $choice))))))

  (define (primitive-replace $replaced-hole $replacement-term $primitive)
    (switch $primitive
      ((number? $number) $number)
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
            (class-args $class))))
      ((tuple? $tuple)
        (tuple
          (map
            (partial term-replace primitive-replace $replaced-hole $replacement-term)
            (tuple-args $tuple))))
      ((choice? $choice)
        (choice
          (map
            (partial term-replace primitive-replace $replaced-hole $replacement-term)
            (choice-args $choice))))))

  (define (primitive-generalize $hole $term)
    (term-generalize primitive-replace $hole $term))

  (define (append-primitive-holes $depth $holes $primitive)
    (switch $primitive
      ((number? _) $holes)
      ((arrow? $arrow)
        (lets
          ($holes
            (fold-left
              (partial append-term-holes append-primitive-holes $depth)
              $holes
              (arrow-params $arrow)))
          ($holes
            (option-fold
              (partial append-term-holes append-primitive-holes $depth)
              $holes
              (arrow-param...? $arrow)))
          (append-term-holes append-primitive-holes $depth $holes
            (arrow-result $arrow))))
      ((class? $class)
        (fold-left
          (partial append-term-holes append-primitive-holes $depth)
          $holes
          (class-args $class)))
      ((tuple? $tuple)
        (fold-left
          (partial append-term-holes append-primitive-holes $depth)
          $holes
          (tuple-args $tuple)))
      ((choice? $choice)
        (fold-left
          (partial append-term-holes append-primitive-holes $depth)
          $holes
          (choice-args $choice)))))
)
