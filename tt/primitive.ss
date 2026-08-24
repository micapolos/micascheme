(library (tt primitive)
  (export
    boolean-type-constructor
    number-type-constructor
    char-type-constructor
    string-type-constructor
    datum-type-constructor

    prim
    prim?
    prim-symbol
    prim-ref
    prim=?
    $prim

    arrow
    arrow?
    arrow-params
    arrow-param...?
    arrow-result

    class
    class?
    class-id
    generate-class
    class->syntax

    tuple
    tuple?
    tuple-args

    choice
    choice?
    choice-args

    primitive?
    primitive-switch

    primitive-ground?
    primitive=?
    primitive->datum
    primitive->syntax
    primitive-unify
    primitive-subst-apply
    primitive-replace
    primitive-generalize
    append-primitive-holes

    syntax->primitive

    primitive-apply)
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
    (system)
    (lets)
    (syntax)
    (keyword)
    (tt term))

  (data (arrow params param...? result))
  (data (class id))
  (data (tuple args))
  (data (choice args))
  (data (prim symbol ref))
  (union (primitive prim class arrow tuple choice))

  (define boolean-type-constructor (type-constructor 'boolean (list)))
  (define number-type-constructor (type-constructor 'number (list)))
  (define char-type-constructor (type-constructor 'char (list)))
  (define string-type-constructor (type-constructor 'string (list)))
  (define datum-type-constructor (type-constructor 'datum (list)))

  (define (generate-class $name)
    (class (gensym $name)))

  (define (class=? $lhs $rhs)
    (symbol=?
      (class-id $lhs)
      (class-id $rhs)))

  (define (prim=? $lhs $rhs)
    (symbol=?
      (prim-symbol $lhs)
      (prim-symbol $rhs)))

  (define-rule-syntax ($prim id)
    (prim 'id ($primitive 2 id)))

  (define (class->datum $class)
    (string->symbol (symbol->string (class-id $class))))

  (define (primitive-ground? $primitive)
    (primitive-switch $primitive
      ((prim? _) #t)
      ((class? _) #t)
      ((arrow? $arrow)
        (and
          (terms-ground? primitive-ground? (arrow-params $arrow))
          (or
            (not (arrow-param...? $arrow))
            (term-ground? primitive-ground? (arrow-param...? $arrow)))
          (term-ground? primitive-ground? (arrow-result $arrow))))
      ((tuple? $tuple)
        (terms-ground? primitive-ground? (tuple-args $tuple)))
      ((choice? $choice)
        (terms-ground? primitive-ground? (choice-args $choice)))))

  (define (primitive->datum $depth $primitive)
    (primitive-switch $primitive
      ((prim? $prim) (prim-symbol $prim))
      ((class? $class) (class->datum $class))
      ((arrow? $arrow)
        `(pi
          (
            ,@(terms->datum primitive->datum $depth (arrow-params $arrow))
            ,@(switch (arrow-param...? $arrow)
              ((false? _) (list))
              ((else $param) (list (term->datum primitive->datum $depth $param) '...))))
          ,(term->datum primitive->datum $depth (arrow-result $arrow))))
      ((tuple? $tuple)
        `(tuple
          ,@(terms->datum primitive->datum $depth (tuple-args $tuple))))
      ((choice? $choice)
        `(choice
          ,@(terms->datum primitive->datum $depth (choice-args $choice))))))

  (define (class->syntax $class)
    #`(class '#,(literal->syntax (class-id $class))))

  (define (primitive->syntax $depth $primitive)
    (primitive-switch $primitive
      ((prim? $prim)
        #`($prim #,(literal->syntax (prim-symbol $prim))))
      ((class? $class)
        (class->syntax $class))
      ((arrow? $arrow)
        #`(arrow
          #,(terms->syntax primitive->syntax $depth
            (arrow-params $arrow))
          #,(switch (arrow-param...? $arrow)
            ((false? _) #'#f)
            ((else $param) (term->syntax primitive->syntax $depth $param)))
          #,(term->syntax primitive->syntax $depth (arrow-result $arrow))))
      ((tuple? $tuple)
        #`(tuple
          #,(terms->syntax primitive->syntax $depth (tuple-args $tuple))))
      ((choice? $choice)
        #`(choice
          #,(terms->syntax primitive->syntax $depth (choice-args $choice))))))

  (define (primitive=? $depth $lhs $rhs)
    (primitive-switch $lhs
      ((prim? $lhs)
        (and
          (prim? $rhs)
          (prim=? $lhs $rhs)))
      ((class? $lhs)
        (and
          (class? $rhs)
          (class=? $lhs $rhs)))
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
    (with-term-mismatch $lhs $rhs
      (switch $lhs
        ((prim? $lhs)
          (and
            (prim? $rhs)
            (prim=? $lhs $rhs)
            $subst))
        ((class? $lhs)
          (and
            (class? $rhs)
            (class=? $lhs $rhs)
            $subst))
        ((arrow? $lhs)
          (and
            (arrow? $rhs)
            (lets?
              ($subst
                (terms-unify primitive-unify $subst
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
        ((tuple? $lhs)
          (and
            (tuple? $rhs)
            (terms-unify primitive-unify $subst
              (tuple-args $lhs)
              (tuple-args $rhs))))
        ((choice? $lhs)
          (and
            (choice? $rhs)
            (terms-unify primitive-unify
              $subst
              (choice-args $lhs)
              (choice-args $rhs)))))))

  (define (primitive-subst-apply $subst $primitive)
    (primitive-switch $primitive
      ((prim? $prim) $prim)
      ((class? $class) $class)
      ((arrow? $arrow)
        (arrow
          (subst-apply* primitive-subst-apply $subst
            (arrow-params $arrow))
          (option-map
            (partial subst-apply primitive-subst-apply $subst)
            (arrow-param...? $arrow))
          (subst-apply primitive-subst-apply $subst
            (arrow-result $arrow))))
      ((tuple? $tuple)
        (tuple
          (subst-apply* primitive-subst-apply $subst
            (tuple-args $tuple))))
      ((choice? $choice)
        (choice
          (subst-apply* primitive-subst-apply $subst
            (choice-args $choice))))))

  (define (primitive-replace $replaced-hole $replacement-term $primitive)
    (switch $primitive
      ((prim? $prim) $prim)
      ((class? $class) $class)
      ((arrow? $arrow)
        (arrow
          (terms-replace primitive-replace $replaced-hole $replacement-term
            (arrow-params $arrow))
          (option-map
            (partial term-replace primitive-replace $replaced-hole $replacement-term)
            (arrow-param...? $arrow))
          (term-replace primitive-replace $replaced-hole $replacement-term
            (arrow-result $arrow))))
      ((tuple? $tuple)
        (tuple
          (terms-replace primitive-replace $replaced-hole $replacement-term
            (tuple-args $tuple))))
      ((choice? $choice)
        (choice
          (terms-replace primitive-replace $replaced-hole $replacement-term
            (choice-args $choice))))))

  (define (primitive-generalize $hole $term)
    (term-generalize primitive-replace $hole $term))

  (define (append-primitive-holes $depth $holes $primitive)
    (switch $primitive
      ((prim? _) $holes)
      ((class? $class) $holes)
      ((arrow? $arrow)
        (lets
          ($holes
            (append-terms-holes append-primitive-holes $depth
              $holes
              (arrow-params $arrow)))
          ($holes
            (option-fold
              (partial append-term-holes append-primitive-holes $depth)
              $holes
              (arrow-param...? $arrow)))
          (append-term-holes append-primitive-holes $depth $holes
            (arrow-result $arrow))))
      ((tuple? $tuple)
        (append-terms-holes append-primitive-holes $depth
          $holes
          (tuple-args $tuple)))
      ((choice? $choice)
        (append-terms-holes append-primitive-holes $depth
          $holes
          (choice-args $choice)))))

  (define (primitive-apply $target $args)
    (cond
      ((and (prim? $target) (for-all (partial term-ground? primitive-ground?) $args))
        (apply (prim-ref $target) $args))
      (else
        (fold-left application $target $args))))

  (define (syntax->primitive $syntax->obj $lookup $syntax)
    (syntax-case $syntax ()
      (b
        (boolean? (datum b))
        (datum b))
      (n
        (number? (datum n))
        (datum n))
      (ch
        (char? (datum ch))
        (datum ch))
      (s
        (string? (datum s))
        (datum s))
      ((call target args ...)
        (free-keyword? call)
        (lets
          ($target ($syntax->obj $lookup #'target))
          ($args (map (partial $syntax->obj $lookup) #'(args ...)))
          (cond
            ((and (prim? $target) (for-all primitive? $args))
              (apply (prim-ref $target) $args))
            (else
              (fold-left application $target $args)))))
      (_
        ($syntax->obj $lookup $syntax))))
)
