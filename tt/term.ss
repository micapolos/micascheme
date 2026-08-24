(library (tt term)
  (export
    index?
    index+1
    syntax->index

    constant?

    kind
    kind?
    kind-index
    kind=?
    syntax->kind

    variable
    variable?
    variable-index
    variable=?

    abstraction
    abstraction?
    abstraction-procedure
    abstraction-apply
    abstraction*
    syntax->abstraction

    product
    product?
    product-domain
    product-procedure
    product-apply
    product*

    application
    application?
    application-lhs
    application-rhs
    application*
    syntax->application

    hole
    hole?
    hole-index
    hole=?

    primitive-application
    primitive-application?
    primitive-application-symbol
    primitive-application-args
    primitive-term

    type-constructor
    type-constructor?
    type-constructor-symbol
    type-constructor-args
    type-term

    tuple-constructor
    tuple-constructor?
    tuple-constructor-args
    tuple-term

    tuple-projection
    tuple-projection?
    tuple-projection-lhs
    tuple-projection-index
    tuple-ref-term

    union-constructor
    union-constructor?
    union-constructor-index
    union-constructor-rhs
    union-term

    union-eliminator
    union-eliminator?
    union-eliminator-lhs
    union-eliminator-branches
    union-case-term

    blank
    blank?

    unified
    unified?
    unified-subst
    unified-ref
    unified-map

    term?
    term-switch
    term/obj?
    syntax->term

    term-ground?
    terms-ground?
    index->datum
    term=?
    term->datum
    terms->datum
    subst->datum
    term->syntax
    terms->syntax
    term-apply
    term-unify
    terms-unify
    subst-resolve
    subst-apply
    subst-apply*
    term-instantiate
    append-term-holes
    append-terms-holes
    term-replace
    terms-replace
    term-generalize
    term-generalize*
    term-intersect
    term-finalize
    arity-term

    &term-mismatch
    make-term-mismatch
    term-mismatch?
    term-mismatch-expected
    term-mismatch-actual
    with-term-mismatch

    native-abstraction)
  (import
    (scheme)
    (procedure)
    (data)
    (lets)
    (except (list) product)
    (switch)
    (boolean)
    (union)
    (syntax)
    (syntaxes)
    (keyword)
    (condition)
    (tt lookup)
    (source-object)
    (prefix (tt keywords) %))

  (data (kind index))
  (data (variable index))
  (data (abstraction procedure))
  (data (product domain procedure))
  (data (application lhs rhs))
  (data (hole index))
  (data (primitive-application symbol args))
  (data (type-constructor symbol args))
  (data (tuple-constructor args))
  (data (tuple-projection lhs index))
  (data (union-constructor index rhs))
  (data (union-eliminator lhs branches))
  (union (term
    kind
    variable
    abstraction
    product
    application
    hole
    type-constructor
    tuple-constructor
    tuple-projection
    union-constructor
    union-eliminator
    primitive-application
    constant))

  (data blank)
  (data (unified subst ref))

  (define (constant? $obj)
    (or
      (boolean? $obj)
      (number? $obj)
      (char? $obj)
      (string? $obj)))

  (define (index? $obj)
    (and
      (integer? $obj)
      (nonnegative? $obj)))

  (define (index+1 $index)
    (fx+ $index 1))

  (define (term/obj? $obj? $x)
    (or
      (term? $x)
      ($obj? $x)))

  (define (unified-map $fn $unified)
    (unified
      (unified-subst $unified)
      ($fn (unified-ref $unified))))

  (define (variable=? $lhs $rhs)
    (=
      (variable-index $lhs)
      (variable-index $rhs)))

  (define (kind=? $lhs $rhs)
    (=
      (kind-index $lhs)
      (kind-index $rhs)))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

  (define (product-apply $product $arg)
    ((product-procedure $product) $arg))

  (define (term-apply $lhs . $rhss)
    (fold-left
      (lambda ($lhs $rhs)
        (switch $lhs
          ((abstraction? $lhs)
            (abstraction-apply $lhs $rhs))
          ((else $lhs)
            (application $lhs $rhs))))
      $lhs
      $rhss))

  (define-rule-syntax (primitive-term obj-ground? id param ...)
    (abstraction* param ...
      (cond
        ((and (term-ground? obj-ground? param) ...)
          (($primitive 2 id) param ...))
        (else
          (primitive-application 'id (list param ...))))))

  (define-rule-syntax (type-term id param ...)
    (abstraction* param ...
      (type-constructor 'id (list param ...))))

  (define-rule-syntax (tuple-term param ...)
    (abstraction* param ...
      (tuple-constructor (list param ...))))

  (define-rule-syntax (tuple-ref-term obj-ground? index)
    (abstraction* id
      (cond
        ((term-ground? obj-ground? id)
          (list-ref (tuple-constructor-args id) index))
        (else
          (tuple-projection id index)))))

  (define-rule-syntax (union-term index param)
    (abstraction* param
      (union-constructor index param)))

  (define-rule-syntax (union-case-term obj-ground? param branch ...)
    (abstraction* param branch ...
      (if (term-ground? obj-ground? param)
        (lets
          ($index (union-constructor-index param))
          ($branch (index-switch $index branch ...))
          (if (term-ground? obj-ground? $branch)
            (abstraction-apply $branch (union-constructor-rhs param))
            (union-eliminator param (list branch ...))))
        (union-eliminator param (list branch ...)))))

  (define (hole=? $lhs $rhs)
    (=
      (hole-index $lhs)
      (hole-index $rhs)))

  (define (index->datum $prefix $depth)
    (string->symbol
      (string-append $prefix
        (number->string $depth))))

  (define (variable->datum $variable)
    (index->datum "$" (variable-index $variable)))

  (define (hole->datum $hole)
    (index->datum "?" (hole-index $hole)))

  (define (fold-abstraction-params $params $depth $term)
    (switch $term
      ((abstraction? $abstraction)
        (lets
          ($variable (variable $depth))
          (fold-abstraction-params
            (cons (variable->datum $variable) $params)
            (+ $depth 1)
            (abstraction-apply $abstraction $variable))))
      ((else _) $params)))

  (define (abstraction->params $depth $term)
    (reverse (fold-abstraction-params (list) $depth $term)))

  (define (abstraction-body->datum $obj->datum $depth $term)
    (switch $term
      ((abstraction? $abstraction)
        (abstraction-body->datum
          $obj->datum
          (+ $depth 1)
          (abstraction-apply $abstraction (variable $depth))))
      ((else $term)
        (term->datum $obj->datum $depth $term))))

  (define (fold-product-domains $obj->datum $params $depth $term)
    (switch $term
      ((product? $product)
        (lets
          ($variable (variable $depth))
          ($param
            `(
              ,(variable->datum $variable)
              ,(term->datum $obj->datum $depth (product-domain $product))))
          (fold-product-domains
            $obj->datum
            (cons $param $params)
            (+ $depth 1)
            (product-apply $product $variable))))
      ((else _) $params)))

  (define (product->params $obj->datum $depth $term)
    (reverse (fold-product-domains $obj->datum (list) $depth $term)))

  (define (product-body->datum $obj->datum $depth $term)
    (switch $term
      ((product? $product)
        (product-body->datum
          $obj->datum
          (+ $depth 1)
          (product-apply $product (variable $depth))))
      ((else $term)
        (term->datum $obj->datum $depth $term))))

  (define (fold-term-arguments $arguments $term)
    (switch $term
      ((application? $application)
        (fold-term-arguments
          (cons (application-rhs $application) $arguments)
          (application-lhs $application)))
      ((else _)
        (cons $term $arguments))))

  (define (term-arguments $term)
    (fold-term-arguments (list) $term))

  (define (symbol->datum $symbol)
    (string->symbol (symbol->string $symbol)))

  (define (terms->datum $obj->datum $depth $terms)
    (map (partial term->datum $obj->datum $depth) $terms))

  (define (terms-ground? $obj-ground? $terms)
    (for-all (partial term-ground? $obj-ground?) $terms))

  (define (term-ground? $obj-ground? $term)
    (term-switch $term
      ((kind? _) #t)
      ((variable? _) #f)
      ((abstraction? _) #t)
      ((product? $product)
        (term-ground? $obj-ground? (product-domain $product)))
      ((application? $application) #f)
      ((hole? _) #f)
      ((type-constructor? $type-constructor)
        (terms-ground? $obj-ground?
          (type-constructor-args $type-constructor)))
      ((tuple-constructor? $tuple-constructor)
        (terms-ground? $obj-ground?
          (tuple-constructor-args $tuple-constructor)))
      ((tuple-projection? $tuple-projection)
        (term-ground? $obj-ground?
          (tuple-projection-lhs $tuple-projection)))
      ((union-constructor? $union-constructor)
        (term-ground? $obj-ground?
          (union-constructor-rhs $union-constructor)))
      ((union-eliminator? $union-eliminator)
        (and
          (term-ground? $obj-ground?
            (union-eliminator-lhs $union-eliminator))
          (terms-ground? $obj-ground?
            (union-eliminator-branches $union-eliminator))))
      ((primitive-application? _) #f)
      ((constant? _) #t)
      ((else $obj) ($obj-ground? $obj))))

  (define (term->datum $obj->datum $depth $term)
    (term-switch $term
      ((kind? $kind)
        `(kind ,(kind-index $kind)))
      ((variable? $variable)
        (variable->datum $variable))
      ((abstraction? $abstraction)
        `(forall
          ,(abstraction->params $depth $abstraction)
          ,(abstraction-body->datum $obj->datum $depth $abstraction)))
      ((product? $product)
        `(pi
          ,(product->params $obj->datum $depth $product)
          ,(product-body->datum $obj->datum $depth $product)))
      ((application? $application)
        (terms->datum $obj->datum $depth (term-arguments $application)))
      ((hole? $hole)
        (hole->datum $hole))
      ((type-constructor? $type-constructor)
        (lets
          ($symbol-datum (symbol->datum (type-constructor-symbol $type-constructor)))
          (cond
            ((null? (type-constructor-args $type-constructor))
              $symbol-datum)
            (else
              `(
                ,$symbol-datum
                ,@(map
                  (partial term->datum $obj->datum $depth)
                  (type-constructor-args $type-constructor)))))))
      ((tuple-constructor? $tuple-constructor)
        `(tuple
          ,@(map
            (partial term->datum $obj->datum $depth)
            (tuple-constructor-args $tuple-constructor))))
      ((tuple-projection? $tuple-projection)
        `(tuple-ref
          ,(term->datum $obj->datum $depth (tuple-projection-lhs $tuple-projection))
          ,(tuple-projection-index $tuple-projection)))
      ((union-constructor? $union-constructor)
        `(union
          ,(union-constructor-index $union-constructor)
          ,(term->datum $obj->datum $depth (union-constructor-rhs $union-constructor))))
      ((union-eliminator? $union-eliminator)
        `(union-case
          ,(term->datum $obj->datum $depth (union-eliminator-lhs $union-eliminator))
          ,@(terms->datum $obj->datum $depth (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        `(
          ,(primitive-application-symbol $primitive-application)
          ,@(map
            (partial term->datum $obj->datum $depth)
            (primitive-application-args $primitive-application))))
      ((constant? $constant)
        $constant)
      ((else $obj)
        ($obj->datum $depth $obj))))

  (define (subst->datum $obj->datum $subst)
    `(subst
      ,@(map
        (lambda ($obj)
          (switch $obj
            ((blank? _) 'blank)
            ((else $term) (term->datum $obj->datum 0 $term))))
        $subst)))

  (define (variable->syntax $variable)
    (literal->syntax
      (string->symbol
        (string-append "$"
          (number->string (variable-index $variable))))))

  (define (terms->syntax $obj->syntax $depth $terms)
    #`(list
      #,@(map
        (partial term->syntax $obj->syntax $depth)
        $terms)))

  (define (term->syntax $obj->syntax $depth $term)
    (term-switch $term
      ((kind? $kind)
        #`(kind #,(literal->syntax (kind-index $kind))))
      ((variable? $variable)
        (variable->syntax $variable))
      ((abstraction? $abstraction)
        (lets
          ($variable (variable $depth))
          #`(abstraction
            (lambda (#,(variable->syntax $variable))
              #,(term->syntax $obj->syntax
                (+ $depth 1)
                (abstraction-apply $abstraction $variable))))))
      ((product? $product)
        (lets
          ($variable (variable $depth))
          #`(product
            #,(term->syntax $obj->syntax $depth (product-domain $product))
            (lambda (#,(variable->syntax $variable))
              #,(term->syntax $obj->syntax
                (+ $depth 1)
                (product-apply $product $variable))))))
      ((application? $application)
        #`(application
          #,(term->syntax $obj->syntax $depth (application-lhs $application))
          #,(term->syntax $obj->syntax $depth (application-rhs $application))))
      ((hole? $hole)
        #`(hole
          #,(literal->syntax (hole-index $hole))))
      ((type-constructor? $type-constructor)
        #`(type-constructor
          '#,(literal->syntax (type-constructor-symbol $type-constructor))
          #,(terms->syntax $obj->syntax $depth
            (type-constructor-args $type-constructor))))
      ((tuple-constructor? $tuple-constructor)
        #`(tuple-constructor
          #,(terms->syntax $obj->syntax $depth
            (tuple-constructor-args $tuple-constructor))))
      ((tuple-projection? $tuple-projection)
        #`(tuple-projection
          #,(term->syntax $obj->syntax $depth
            (tuple-projection-lhs $tuple-projection))
          #,(literal->syntax
            (tuple-projection-index $tuple-projection))))
      ((union-constructor? $union-constructor)
        #`(union-constructor
          #,(literal->syntax (union-constructor-index $union-constructor))
          #,(term->syntax $obj->syntax $depth (union-constructor-rhs $union-constructor))))
      ((union-eliminator? $union-eliminator)
        #`(union-eliminator
          #,(term->syntax $obj->syntax $depth (union-eliminator-lhs $union-eliminator))
          #,(terms->syntax $obj->syntax $depth (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        #`(primitive-application
          ($primitive 2 #,(literal->syntax (primitive-application-symbol $primitive-application)))
          #,(terms->syntax $obj->syntax $depth
            (primitive-application-args $primitive-application))))
      ((constant? $constant)
        (literal->syntax $constant))
      ((else $obj)
        ($obj->syntax $depth $obj))))

  (define (term=? $obj=? $depth $lhs $rhs)
    (term-switch $lhs
      ((kind? $lhs)
        (and
          (kind? $rhs)
          (kind=? $lhs $rhs)))
      ((variable? $lhs)
        (and
          (variable $rhs)
          (variable=? $lhs $rhs)))
      ((abstraction? $lhs)
        (and
          (abstraction? $rhs)
          (term=? $obj=? (+ $depth 1)
            (abstraction-apply $lhs (hole $depth))
            (abstraction-apply $rhs (hole $depth)))))
      ((product? $lhs)
        (and
          (product? $rhs)
          (term=? $obj=? $depth
            (product-domain $lhs)
            (product-domain $rhs))
          (term=? $obj=? (+ $depth 1)
            (product-apply $lhs (hole $depth))
            (product-apply $rhs (hole $depth)))))
      ((application? $lhs)
        (and
          (application? $rhs)
          (term=? $obj=? $depth
            (application-lhs $lhs)
            (application-lhs $rhs))
          (term=? $obj=? $depth
            (application-rhs $lhs)
            (application-rhs $rhs))))
      ((hole? $lhs)
        (and
          (hole? $rhs)
          (hole=? $lhs $rhs)))
      ((type-constructor? $lhs)
        (and
          (type-constructor? $rhs)
          (symbol=?
            (type-constructor-symbol $lhs)
            (type-constructor-symbol $rhs))
          (for-all* (partial term=? $obj=? $depth)
            (type-constructor-args $lhs)
            (type-constructor-args $rhs))))
      ((tuple-constructor? $lhs)
        (and
          (tuple-constructor? $rhs)
          (for-all* (partial term=? $obj=? $depth)
            (tuple-constructor-args $lhs)
            (tuple-constructor-args $rhs))))
      ((tuple-projection? $lhs)
        (and
          (tuple-projection? $rhs)
          (term=? $obj=? $depth
            (tuple-projection-lhs $lhs)
            (tuple-projection-lhs $rhs))
          (=
            (tuple-projection-index $lhs)
            (tuple-projection-index $rhs))))
      ((union-constructor? $lhs)
        (and
          (union-constructor? $rhs)
          (=
            (union-constructor-index $lhs)
            (union-constructor-index $rhs))
          (term=? $obj=? $depth
            (union-constructor-rhs $lhs)
            (union-constructor-rhs $rhs))))
      ((union-eliminator? $lhs)
        (and
          (union-eliminator? $rhs)
          (term=? $obj=? $depth
            (union-eliminator-lhs $lhs)
            (union-eliminator-lhs $rhs))
          (for-all* (partial term=? $obj=? $depth)
            (union-eliminator-branches $lhs)
            (union-eliminator-branches $rhs))))
      ((primitive-application? $lhs)
        (and
          (primitive-application? $rhs)
          (symbol=?
            (primitive-application-symbol $lhs)
            (primitive-application-symbol $rhs))
          (for-all* (partial term=? $obj=? $depth)
            (primitive-application-args $lhs)
            (primitive-application-args $rhs))))
      ((constant? $lhs)
        (and
          (constant? $rhs)
          (equal? $lhs $rhs)))
      ((else $lhs)
        ($obj=? $depth $lhs $rhs))))

  (define (subst-index $subst $hole)
    (- (length $subst) (hole-index $hole) 1))

  (define (subst-ref $subst $hole)
    (list-ref $subst (subst-index $subst $hole)))

  (define (subst-set $subst $hole $type)
    (list-set $subst (subst-index $subst $hole) $type))

  (define (subst-resolve $subst $term)
    (switch $term
      ((hole? $term)
        (switch (subst-ref $subst $term)
          ((blank? _) $term)
          ((else $term) (subst-resolve $subst $term))))
      ((else $term) $term)))

  (define (subst-alloc $subst)
    (lets
      ($index (length $subst))
      ($subst (cons blank $subst))
      (values $subst (make-hole $index))))

  (define-condition-type &term-mismatch &violation make-term-mismatch term-mismatch?
    (expected term-mismatch-expected)
    (actual term-mismatch-actual))

  (define-rule-syntax (with-term-mismatch expected actual body)
    (or
      body
      (raise (make-term-mismatch expected actual))))

  (define (terms-unify $obj-unify $subst $lhss $rhss)
    (and
      (= (length $lhss) (length $rhss))
      (fold-left (partial term-unify $obj-unify) $subst $lhss $rhss)))

  (define (term-unify $obj-unify $subst $lhs $rhs)
    (lets
      ($lhs (subst-resolve $subst $lhs))
      ($rhs (subst-resolve $subst $rhs))
      (with-term-mismatch $lhs $rhs
        (cond
          ((and (hole? $lhs) (hole? $rhs))
            (cond
              ((= (hole-index $lhs) (hole-index $rhs)) $subst)
              (else (subst-set $subst $lhs $rhs))))

          ((hole? $lhs) (subst-set $subst $lhs $rhs))
          ((hole? $rhs) (subst-set $subst $rhs $lhs))

          ((abstraction? $lhs)
            (lets
              ((values $subst $hole) (subst-alloc $subst))
              (term-unify $obj-unify $subst (abstraction-apply $lhs $hole) $rhs)))

          ((abstraction? $rhs)
            (lets
              ((values $subst $hole) (subst-alloc $subst))
              (term-unify $obj-unify $subst $lhs (abstraction-apply $rhs $hole))))

          ((kind? $lhs)
            (and
              (kind? $rhs)
              (kind=? $lhs $rhs)
              $subst))

          ((variable? $lhs)
            (and
              (variable? $rhs)
              (variable=? $lhs $rhs)
              $subst))

          ((product? $lhs)
            (and
              (product? $rhs)
              (lets
                ($subst
                  (term-unify $obj-unify $subst
                    (product-domain $lhs)
                    (product-domain $rhs)))
                ((values $subst $lhs-hole) (subst-alloc $subst))
                ((values $subst $rhs-hole) (subst-alloc $subst))
                (term-unify $obj-unify $subst
                  (product-apply $lhs $lhs-hole)
                  (product-apply $rhs $rhs-hole)))))

          ((application? $lhs)
            (and
              (application? $rhs)
              (lets
                ($subst
                  (term-unify $obj-unify $subst
                    (application-lhs $lhs)
                    (application-lhs $rhs)))
                (term-unify $obj-unify $subst
                  (application-rhs $lhs)
                  (application-rhs $rhs)))))

          ((type-constructor? $lhs)
            (and
              (type-constructor? $rhs)
              (symbol=?
                (type-constructor-symbol $lhs)
                (type-constructor-symbol $rhs))
              (terms-unify $obj-unify $subst
                (type-constructor-args $lhs)
                (type-constructor-args $rhs))))

          ((tuple-constructor? $lhs)
            (and
              (tuple-constructor? $rhs)
              (terms-unify $obj-unify $subst
                (tuple-constructor-args $lhs)
                (tuple-constructor-args $rhs))))

          ((tuple-projection? $lhs)
            (and
              (tuple-projection? $rhs)
              (=
                (tuple-projection-index $lhs)
                (tuple-projection-index $rhs))
              (terms-unify $obj-unify $subst
                (tuple-projection-lhs $lhs)
                (tuple-projection-lhs $rhs))))

          ((union-constructor? $lhs)
            (and
              (union-constructor? $rhs)
              (=
                (union-constructor-index $lhs)
                (union-constructor-index $rhs))
              (terms-unify $obj-unify $subst
                (union-constructor-rhs $lhs)
                (union-constructor-rhs $rhs))))

          ((union-eliminator? $lhs)
            (and
              (union-eliminator? $rhs)
              (lets
                ($subst
                  (term-unify $obj-unify $subst
                    (union-eliminator-lhs $lhs)
                    (union-eliminator-lhs $rhs)))
                (terms-unify $obj-unify $subst
                  (union-eliminator-branches $lhs)
                  (union-eliminator-branches $rhs)))))

          ((primitive-application? $lhs)
            (and
              (primitive-application? $rhs)
              (symbol=?
                (primitive-application-symbol $lhs)
                (primitive-application-symbol $rhs))
              (terms-unify $obj-unify $subst
                (primitive-application-args $lhs)
                (primitive-application-args $rhs))))

          ((constant? $lhs)
            (and
              (constant? $rhs)
              (equal? $lhs $rhs)
              $subst))

          (else
            ($obj-unify $subst $lhs $rhs))))))

  (define (term-instantiate $subst $term)
    (lets
      ($term (subst-resolve $subst $term))
      (cond
        ((abstraction? $term)
          (lets
            ((values $subst $hole) (subst-alloc $subst))
            (term-instantiate $subst (abstraction-apply $term $hole))))
        (else
          (values $subst $term)))))

  (define (subst-apply* $obj-apply $subst $terms)
    (map (partial subst-apply $obj-apply $subst) $terms))

  (define (subst-apply $obj-apply $subst $term)
    (lets
      ($term (subst-resolve $subst $term))
      (term-switch $term
        ((kind? $kind) $kind)
        ((variable? $variable) $variable)
        ((abstraction? $abstraction)
          (abstraction
            (lambda ($arg)
              (subst-apply $obj-apply $subst
                (abstraction-apply $abstraction $arg)))))
        ((product? $product)
          (product
            (subst-apply $obj-apply $subst
              (product-domain $product))
            (lambda ($arg)
              (subst-apply $obj-apply $subst
                (product-apply $product $arg)))))
        ((application? $application)
          (application
            (subst-apply $obj-apply $subst
              (application-lhs $application))
            (subst-apply $obj-apply $subst
              (application-rhs $application))))
        ((hole? $hole) $hole)
        ((type-constructor? $type-constructor)
          (type-constructor
            (type-constructor-symbol $type-constructor)
            (subst-apply* $obj-apply $subst
              (type-constructor-args $type-constructor))))
        ((tuple-constructor? $tuple-constructor)
          (tuple-constructor
            (subst-apply* $obj-apply $subst
              (tuple-constructor-args $tuple-constructor))))
        ((tuple-projection? $tuple-projection)
          (tuple-projection
            (subst-apply $obj-apply $subst
              (tuple-projection-lhs $tuple-projection))
            (tuple-projection-index $tuple-projection)))
        ((union-constructor? $union-constructor)
          (union-constructor
            (union-constructor-index $union-constructor)
            (subst-apply $obj-apply $subst
              (union-constructor-rhs $union-constructor))))
        ((union-eliminator? $union-eliminator)
          (union-eliminator
            (subst-apply $obj-apply $subst
              (union-eliminator-lhs $union-eliminator))
            (subst-apply* $obj-apply $subst
              (union-eliminator-branches $union-eliminator))))
        ((primitive-application? $primitive-application)
          (primitive-application
            (primitive-application-symbol $primitive-application)
            (subst-apply* $obj-apply $subst
              (primitive-application-args $primitive-application))))
        ((constant? $constant) $constant)
        ((else $obj)
          ($obj-apply $subst $obj)))))

  (define (terms-replace $obj-replace $replaced-hole $replacement-term $terms)
    (map
      (partial term-replace $obj-replace $replaced-hole $replacement-term)
      $terms))

  (define (term-replace $obj-replace $replaced-hole $replacement-term $term)
    (term-switch $term
      ((kind? $kind) $kind)
      ((variable? $variable) $variable)
      ((abstraction? $abstraction)
        (abstraction
          (lambda ($arg)
            (term-replace
              $obj-replace
              $replaced-hole
              $replacement-term
              (abstraction-apply $abstraction $arg)))))
      ((product? $product)
        (product
          (term-replace $obj-replace
            $replaced-hole
            $replacement-term
            (product-domain $product))
          (lambda ($arg)
            (term-replace
              $obj-replace
              $replaced-hole
              $replacement-term
              (product-apply $product $arg)))))
      ((application? $application)
        (application
          (term-replace $obj-replace
            $replaced-hole
            $replacement-term
            (application-lhs $application))
          (term-replace $obj-replace
            $replaced-hole
            $replacement-term
            (application-rhs $application))))
      ((hole? $hole)
        (cond
          ((hole=? $hole $replaced-hole) $replacement-term)
          (else $hole)))
      ((type-constructor? $type-constructor)
        (type-constructor
          (type-constructor-symbol $type-constructor)
          (terms-replace $obj-replace $replaced-hole $replacement-term
            (type-constructor-args $type-constructor))))
      ((tuple-constructor? $tuple-constructor)
        (tuple-constructor
          (terms-replace $obj-replace $replaced-hole $replacement-term
            (tuple-constructor-args $tuple-constructor))))
      ((tuple-projection? $tuple-projection)
        (tuple-projection
          (term-replace $obj-replace $replaced-hole $replacement-term
            (tuple-projection-lhs $tuple-projection))
          (tuple-projection-index $tuple-projection)))
      ((union-constructor? $union-constructor)
        (union-constructor
          (union-constructor-index $union-constructor)
          (terms-replace $obj-replace $replaced-hole $replacement-term
            (union-constructor-rhs $union-constructor))))
      ((union-eliminator? $union-eliminator)
        (union-eliminator
          (term-replace $obj-replace $replaced-hole $replacement-term
            (union-eliminator-lhs $union-eliminator))
          (terms-replace $obj-replace $replaced-hole $replacement-term
            (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        (primitive-application
          (primitive-application-symbol $primitive-application)
          (terms-replace $obj-replace $replaced-hole $replacement-term
            (primitive-application-args $primitive-application))))
      ((constant? $constant) $constant)
      ((else $obj)
        ($obj-replace $replaced-hole $replacement-term $term))))

  (define (append-terms-holes $append-obj-holes $depth $holes $terms)
    (fold-left
      (partial append-term-holes $append-obj-holes $depth)
      $holes $terms))

  (define (append-term-holes $append-obj-holes $depth $holes $term)
    (term-switch $term
      ((kind? _) $holes)
      ((variable? _) $holes)
      ((abstraction? $abstraction)
        (append-term-holes $append-obj-holes (+ $depth 1) $holes
          (abstraction-apply $abstraction (variable $depth))))
      ((product? $product)
        (lets
          ($holes
            (append-term-holes $append-obj-holes $depth $holes
              (product-domain $product)))
          (append-term-holes $append-obj-holes (+ $depth 1) $holes
            (product-apply $product (variable $depth)))))
      ((application? $application)
        (lets
          ($holes
            (append-term-holes $append-obj-holes $depth $holes
              (application-lhs $application)))
          (append-term-holes $append-obj-holes $depth $holes
            (application-rhs $application))))
      ((hole? $hole)
        (cons/nodup hole=? $hole $holes))
      ((type-constructor? $type-constructor)
        (append-terms-holes $append-obj-holes $depth
          $holes
          (type-constructor-args $type-constructor)))
      ((tuple-constructor? $tuple-constructor)
        (append-terms-holes $append-obj-holes $depth
          $holes
          (tuple-constructor-args $tuple-constructor)))
      ((tuple-projection? $tuple-projection)
        (append-term-holes $append-obj-holes $depth
          $holes
          (tuple-projection-lhs $tuple-projection)))
      ((union-constructor? $union-constructor)
        (append-term-holes $append-obj-holes $depth
          $holes
          (union-constructor-rhs $union-constructor)))
      ((union-eliminator? $union-eliminator)
        (lets
          ($holes
            (append-term-holes $append-obj-holes $depth $holes
              (union-eliminator-lhs $union-eliminator)))
          (append-terms-holes $append-obj-holes $depth $holes
            (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        (append-terms-holes $append-obj-holes $depth
          $holes
          (primitive-application-args $primitive-application)))
      ((constant? _) $holes)
      ((else $obj)
        ($append-obj-holes $depth $holes $obj))))

  (define (term-generalize $obj-replace $hole $term)
    (abstraction
      (lambda ($arg)
        (term-replace $obj-replace $hole $arg $term))))

  (define (term-generalize* $obj-replace $holes $term)
    (fold-left
      (lambda ($term $hole)
        (term-generalize $obj-replace $hole $term))
      $term
      (reverse $holes)))

  (define (application* $lhs . $rhss)
    (fold-left application $lhs $rhss))

  (define-rules-syntax
    ((abstraction* body) body)
    ((abstraction* param param* ... body)
      (abstraction
        (lambda (param)
          (abstraction* param* ... body)))))

  (define-rules-syntax
    ((product* body) body)
    ((product* (id t) params ... body)
      (product t
        (lambda (id)
          (product* params ... body)))))

  (define (arity-term $obj-replace $arity $procedure)
    (lets
      ($indices (iota $arity))
      ($holes (map hole $indices))
      (term-generalize* $obj-replace $holes ($procedure $holes))))

  (define-rule-syntax (native-abstraction obj->apply id param ...)
    (abstraction* param ...
      (cond
        ((and (not (term? id)) (not (term? param)) ...)
          (obj->apply id param ...))
        (else
          (application* id param ...)))))

  (define (term-intersect $obj-unify $append-obj-holes $obj-apply $obj-replace $lhs $rhs)
    (lets
      ((values $subst $lhs) (term-instantiate (list) $lhs))
      ($subst (term-unify $obj-unify $subst $lhs $rhs))
      ($lhs (subst-apply $obj-apply $subst $lhs))
      ($holes (append-term-holes $append-obj-holes 0 (list) $lhs))
      (fold-left
        (lambda ($term $hole) (term-generalize $obj-replace $hole $term))
        $lhs
        $holes)))

  (define (term-finalize $obj-apply $append-obj-holes $obj-replace $subst $term)
    (lets
      ($term (subst-apply $obj-apply $subst $term))
      ($holes (append-term-holes $append-obj-holes 0 (list) $term))
      (term-generalize* $obj-replace (reverse $holes) $term)))

  (define (syntax->index $syntax)
    (syntax-case $syntax ()
      (i
        (index? (datum i))
        (datum i))))

  (define (syntax->kind $syntax)
    (syntax-case $syntax ()
      ((_ index)
        (kind (syntax->index #'index)))))

  (define (syntax->hole $syntax)
    (syntax-case $syntax ()
      ((_ index)
        (hole (syntax->index #'index)))))

  (define (syntax->variable $lookup $syntax)
    (syntax-case $syntax ()
      (id
        (switch ($lookup #'id)
          ((false? _) (syntax-error #'id "unbound variable"))
          ((else $other) $other)))))

  (define (syntax->lookup-push $lookup $syntax $arg)
    (syntax-case $syntax ()
      (id
        (keyword? id)
        (cond
          ((free-identifier=? #'id #'_) $lookup)
          (else (lookup-push $lookup #'id $arg))))))

  (define (syntax->abstraction $syntax->obj $lookup $syntax)
    (syntax-case $syntax ()
      ((_ () body)
        (syntax->term $syntax->obj $lookup #'body))
      ((_ (id . x) body)
        (abstraction
          (lambda ($arg)
            (syntax->term $syntax->obj
              (syntax->lookup-push $lookup #'id $arg)
              #'(lambda x body)))))))

  (define (syntax->product $syntax->obj $lookup $syntax)
    (syntax-case $syntax ()
      ((_ () body)
        (syntax->term $syntax->obj $lookup #'body))
      ((_ ((id t) . x) body)
        (product
          (syntax->term $syntax->obj $lookup #'t)
          (lambda ($arg)
            (syntax->term $syntax->obj
              (syntax->lookup-push $lookup #'id $arg)
              #'(pi x body)))))))

  (define (syntax->application $syntax->obj $lookup $syntax)
    (syntax-case $syntax ()
      ((target args ...)
        (fold-left
          term-apply
          (syntax->term $syntax->obj $lookup #'target)
          (map (partial syntax->term $syntax->obj $lookup) #'(args ...))))))

  (define (syntax->term $syntax->obj $lookup $syntax)
    (syntax-case $syntax ()
      (id
        (keyword? id)
        (syntax->variable $lookup #'id))
      ((kind . x)
        (free-keyword? kind)
        (syntax->kind $syntax))
      ((hole . x)
        (free-keyword? hole)
        (syntax->hole $syntax))
      ((lambda . x)
        (free-keyword? lambda)
        (syntax->abstraction $syntax->obj $lookup $syntax))
      ((pi . x)
        (free-keyword? pi)
        (syntax->product $syntax->obj $lookup $syntax))
      (_
        ($syntax->obj (partial syntax->application $syntax->obj) $lookup $syntax))))
)
