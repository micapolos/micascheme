(library (tt primitive)
  (export
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
  (union (primitive prim boolean number char string class arrow tuple choice))

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

  (define (primitive->datum $depth $primitive)
    (primitive-switch $primitive
      ((prim? $prim) (prim-symbol $prim))
      ((boolean? $boolean) $boolean)
      ((number? $number) $number)
      ((char? $char) $char)
      ((string? $string) $string)
      ((class? $class) (class->datum $class))
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
      ((tuple? $tuple)
        `(tuple
          ,@(map (partial term->datum primitive->datum $depth) (tuple-args $tuple))))
      ((choice? $choice)
        `(choice
          ,@(map (partial term->datum primitive->datum $depth) (choice-args $choice))))))

  (define (class->syntax $class)
    #`(class '#,(literal->syntax (class-id $class))))

  (define (primitive->syntax $depth $primitive)
    (primitive-switch $primitive
      ((prim? $prim)
        #`($prim #,(literal->syntax (prim-symbol $prim))))
      ((boolean? $boolean)
        (literal->syntax $boolean))
      ((number? $number)
        (literal->syntax $number))
      ((char? $char)
        (literal->syntax $char))
      ((string? $string)
        (literal->syntax $string))
      ((class? $class)
        (class->syntax $class))
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
      ((tuple? $tuple)
        #`(tuple
          (list #,@(map (partial term->syntax primitive->syntax $depth) (tuple-args $tuple)))))
      ((choice? $choice)
        #`(choice
          (list #,@(map (partial term->syntax primitive->syntax $depth) (choice-args $choice)))))))

  (define (primitive=? $depth $lhs $rhs)
    (primitive-switch $lhs
      ((prim? $lhs)
        (and
          (prim? $rhs)
          (prim=? $lhs $rhs)))
      ((boolean? $lhs)
        (and
          (boolean? $rhs)
          (boolean=? $lhs $rhs)))
      ((number? $lhs)
        (and
          (number? $rhs)
          (= $lhs $rhs)))
      ((char? $lhs)
        (and
          (char? $rhs)
          (char=? $lhs $rhs)))
      ((string? $lhs)
        (and
          (string? $rhs)
          (string=? $lhs $rhs)))
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
    (switch $lhs
      ((prim? $lhs)
        (and
          (prim? $rhs)
          (prim=? $lhs $rhs)
          $subst))
      ((boolean? $lhs)
        (and
          (boolean? $rhs)
          (boolean=? $lhs $rhs)
          $subst))
      ((number? $lhs)
        (and
          (number? $rhs)
          (= $lhs $rhs)
          $subst))
      ((char? $lhs)
        (and
          (char? $rhs)
          (char=? $lhs $rhs)
          $subst))
      ((string? $lhs)
        (and
          (string? $rhs)
          (string=? $lhs $rhs)
          $subst))
      ((class? $lhs)
        (and
          (class? $rhs)
          (class=? $lhs $rhs)
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
      ((prim? $prim) $prim)
      ((boolean? $boolean) $boolean)
      ((number? $number) $number)
      ((char? $char) $char)
      ((string? $string) $string)
      ((class? $class) $class)
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
      ((prim? $prim) $prim)
      ((boolean? $boolean) $boolean)
      ((number? $number) $number)
      ((char? $char) $char)
      ((string? $string) $string)
      ((class? $class) $class)
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
      ((prim? _) $holes)
      ((boolean? _) $holes)
      ((number? _) $holes)
      ((char? _) $holes)
      ((string? _) $holes)
      ((class? $class) $holes)
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

  (define (primitive-apply $target $args)
    (cond
      ((and (prim? $target) (for-all primitive? $args))
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
