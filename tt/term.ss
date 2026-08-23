(library (tt term)
  (export
    index?
    index+1
    syntax->index

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
    product-param
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

    syntaxed
    syntaxed?
    syntaxed-syntax
    syntaxed-ref

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
    term-stripped

    index->datum
    term=?
    term->datum
    subst->datum
    term->syntax
    term-apply
    term-unify
    subst-resolve
    subst-apply
    term-instantiate
    append-term-holes
    term-replace
    term-generalize
    term-generalize*
    term-intersect
    term-finalize
    arity-term

    &type-violation
    make-type-violation
    type-violation?
    type-violation-expected
    type-violation-actual
    with-type-violation

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
  (data (product param procedure))
  (data (application lhs rhs))
  (data (hole index))
  (data (syntaxed syntax ref))
  (union (term kind variable abstraction product application hole syntaxed))

  (data blank)
  (data (unified subst ref))

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
        (switch (term-stripped $lhs)
          ((abstraction? $lhs)
            (abstraction-apply $lhs $rhs))
          ((else $lhs)
            (application $lhs $rhs))))
      $lhs
      $rhss))

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

  (define (fold-product-params $obj->datum $params $depth $term)
    (switch $term
      ((product? $product)
        (lets
          ($variable (variable $depth))
          ($param
            `(
              ,(variable->datum $variable)
              ,(term->datum $obj->datum $depth (product-param $product))))
          (fold-product-params
            $obj->datum
            (cons $param $params)
            (+ $depth 1)
            (product-apply $product $variable))))
      ((else _) $params)))

  (define (product->params $obj->datum $depth $term)
    (reverse (fold-product-params $obj->datum (list) $depth $term)))

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
        (map
          (partial term->datum $obj->datum $depth)
          (term-arguments $application)))
      ((hole? $hole)
        (hole->datum $hole))
      ((syntaxed? $syntaxed)
        (term->datum $obj->datum $depth
          (syntaxed-ref $syntaxed)))
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
            #,(term->syntax $obj->syntax $depth (product-param $product))
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
      ((syntaxed? $syntaxed)
        #`(syntaxed
          #'#,(syntaxed-syntax $syntaxed)
          #,(term->syntax $obj->syntax $depth (syntaxed-ref $syntaxed))))
      ((else $obj)
        ($obj->syntax $depth $obj))))

  (define (term-stripped $term)
    (switch $term
      ((syntaxed? $syntaxed) (syntaxed-ref $syntaxed))
      ((else $other) $other)))

  (define (term=? $obj=? $index $lhs $rhs)
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
          (term=? $obj=? (+ $index 1)
            (abstraction-apply $lhs (hole $index))
            (abstraction-apply $rhs (hole $index)))))
      ((product? $lhs)
        (and
          (product? $rhs)
          (term=? $obj=? $index
            (product-param $lhs)
            (product-param $rhs))
          (term=? $obj=? (+ $index 1)
            (product-apply $lhs (hole $index))
            (product-apply $rhs (hole $index)))))
      ((application? $lhs)
        (and
          (application? $rhs)
          (term=? $obj=? $index
            (application-lhs $lhs)
            (application-lhs $rhs))
          (term=? $obj=? $index
            (application-rhs $lhs)
            (application-rhs $rhs))))
      ((hole? $lhs)
        (and
          (hole? $rhs)
          (hole=? $lhs $rhs)))
      ((syntaxed? $lhs)
        (term=? $obj=? $index
          (syntaxed-ref $lhs)
          (term-stripped $lhs)))
      ((else $lhs)
        ($obj=? $index $lhs $rhs))))

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

  (define-condition-type &type-violation &violation make-type-violation type-violation?
    (expected type-violation-expected)
    (actual type-violation-actual))

  (define-rule-syntax (with-type-violation expected actual body)
    (or
      body
      (raise (make-type-violation expected actual))))

  (define (term-unify $obj-unify $subst? $lhs $rhs)
    (lets?
      ($subst $subst?)
      (lets
        ($lhs (subst-resolve $subst $lhs))
        ($rhs (subst-resolve $subst $rhs))
        (with-type-violation $lhs $rhs
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
                      (product-param $lhs)
                      (product-param $rhs)))
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

            ((syntaxed? $lhs)
              (term-unify $obj-unify $subst
                (syntaxed-ref $lhs)
                (term-stripped $rhs)))

            (else
              ($obj-unify $subst $lhs $rhs)))))))

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
              (product-param $product))
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
        ((syntaxed? $syntaxed)
          (syntaxed
            (syntaxed-syntax $syntaxed)
            (subst-apply $obj-apply $subst (syntaxed-ref $syntaxed))))
        ((else $obj)
          ($obj-apply $subst $obj)))))

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
            (product-param $product))
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
      ((syntaxed? $syntaxed)
        (syntaxed
          (syntaxed-syntax $syntaxed)
          (term-replace $obj-replace
            $replaced-hole
            $replacement-term
            (syntaxed-ref $syntaxed))))
      ((else $obj)
        ($obj-replace $replaced-hole $replacement-term $term))))

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
              (product-param $product)))
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
      ((syntaxed? $syntaxed)
        (append-term-holes $append-obj-holes $depth $holes
          (syntaxed-ref $syntaxed)))
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
        (syntaxed
          $syntax
          (kind (syntax->index #'index))))))

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
