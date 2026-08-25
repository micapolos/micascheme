(library (tt term)
  (export
    index?
    index+1

    constant?

    kind
    kind?
    kind-index
    kind=?

    variable
    variable?
    variable-index
    variable=?

    abstraction
    abstraction?
    abstraction-procedure
    abstraction-apply
    abstraction*

    pi
    pi?
    pi-domain
    pi-procedure
    pi-apply
    pi*

    application
    application?
    application-lhs
    application-rhs
    application*

    hole
    hole?
    hole-index
    hole-domain
    hole-index=?

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
    term-finalize
    arity-term)
  (import
    (scheme)
    (procedure)
    (data)
    (lets)
    (list)
    (switch)
    (boolean)
    (union)
    (syntax)
    (syntaxes)
    (keyword)
    (condition))

  (data (kind index))
  (data (variable index))
  (data (abstraction procedure))
  (data (pi domain procedure))
  (data (application lhs rhs))
  (data (hole index domain))
  (data (primitive-application symbol args))
  (data (type-constructor symbol args))
  (data (tuple-constructor args))
  (data (tuple-projection lhs index))
  (data (union-constructor index rhs))
  (data (union-eliminator lhs branches))

  (union
    (term
      constant
      kind
      variable
      abstraction
      pi
      application
      hole
      type-constructor
      tuple-constructor
      tuple-projection
      union-constructor
      union-eliminator
      primitive-application))

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

  (define (pi-apply $pi $arg)
    ((pi-procedure $pi) $arg))

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

  (define-rule-syntax (primitive-term id param ...)
    (abstraction* param ...
      (cond
        ((and (term-ground? param) ...)
          (($primitive 2 id) param ...))
        (else
          (primitive-application 'id (list param ...))))))

  (define-rule-syntax (type-term id param ...)
    (abstraction* param ...
      (type-constructor 'id (list param ...))))

  (define-rule-syntax (tuple-term param ...)
    (abstraction* param ...
      (tuple-constructor (list param ...))))

  (define-rule-syntax (tuple-ref-term index)
    (abstraction* id
      (cond
        ((term-ground? id)
          (list-ref (tuple-constructor-args id) index))
        (else
          (tuple-projection id index)))))

  (define-rule-syntax (union-term index param)
    (abstraction* param
      (union-constructor index param)))

  (define-rule-syntax (union-case-term param branch ...)
    (abstraction* param branch ...
      (if (term-ground? param)
        (lets
          ($index (union-constructor-index param))
          ($branch (index-switch $index branch ...))
          (if (term-ground? $branch)
            (abstraction-apply $branch (union-constructor-rhs param))
            (union-eliminator param (list branch ...))))
        (union-eliminator param (list branch ...)))))

  (define (hole-index=? $lhs $rhs)
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

  (define (abstraction-body->datum $depth $term)
    (switch $term
      ((abstraction? $abstraction)
        (abstraction-body->datum
          (+ $depth 1)
          (abstraction-apply $abstraction (variable $depth))))
      ((else $term)
        (term->datum $depth $term))))

  (define (fold-pi-domains $params $depth $term)
    (switch $term
      ((pi? $pi)
        (lets
          ($variable (variable $depth))
          ($param
            `(
              ,(variable->datum $variable)
              ,(term->datum $depth (pi-domain $pi))))
          (fold-pi-domains
            (cons $param $params)
            (+ $depth 1)
            (pi-apply $pi $variable))))
      ((else _) $params)))

  (define (pi->params $depth $term)
    (reverse (fold-pi-domains (list) $depth $term)))

  (define (pi-body->datum $depth $term)
    (switch $term
      ((pi? $pi)
        (pi-body->datum
          (+ $depth 1)
          (pi-apply $pi (variable $depth))))
      ((else $term)
        (term->datum $depth $term))))

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

  (define (terms->datum $depth $terms)
    (map (partial term->datum $depth) $terms))

  (define (terms-ground? $terms)
    (for-all (partial term-ground?) $terms))

  (define (term-ground? $term)
    (term-switch $term
      ((constant? _) #t)
      ((kind? _) #t)
      ((variable? _) #f)
      ((abstraction? _) #t)
      ((pi? $pi)
        (term-ground? (pi-domain $pi)))
      ((application? $application) #f)
      ((hole? _) #f)
      ((type-constructor? $type-constructor)
        (terms-ground?
          (type-constructor-args $type-constructor)))
      ((tuple-constructor? $tuple-constructor)
        (terms-ground?
          (tuple-constructor-args $tuple-constructor)))
      ((tuple-projection? $tuple-projection)
        (term-ground?
          (tuple-projection-lhs $tuple-projection)))
      ((union-constructor? $union-constructor)
        (term-ground?
          (union-constructor-rhs $union-constructor)))
      ((union-eliminator? $union-eliminator)
        (and
          (term-ground?
            (union-eliminator-lhs $union-eliminator))
          (terms-ground?
            (union-eliminator-branches $union-eliminator))))
      ((primitive-application? _) #f)))

  (define (term->datum $depth $term)
    (term-switch $term
      ((constant? $constant)
        $constant)
      ((kind? $kind)
        `(kind ,(kind-index $kind)))
      ((variable? $variable)
        (variable->datum $variable))
      ((abstraction? $abstraction)
        `(forall
          ,(abstraction->params $depth $abstraction)
          ,(abstraction-body->datum $depth $abstraction)))
      ((pi? $pi)
        `(pi
          ,(pi->params $depth $pi)
          ,(pi-body->datum $depth $pi)))
      ((application? $application)
        (terms->datum $depth (term-arguments $application)))
      ((hole? $hole)
        `(hole
          ,(hole-index $hole)
          ,(term->datum $depth (hole-domain $hole))))
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
                  (partial term->datum $depth)
                  (type-constructor-args $type-constructor)))))))
      ((tuple-constructor? $tuple-constructor)
        `(tuple
          ,@(map
            (partial term->datum $depth)
            (tuple-constructor-args $tuple-constructor))))
      ((tuple-projection? $tuple-projection)
        `(tuple-ref
          ,(term->datum $depth (tuple-projection-lhs $tuple-projection))
          ,(tuple-projection-index $tuple-projection)))
      ((union-constructor? $union-constructor)
        `(union
          ,(union-constructor-index $union-constructor)
          ,(term->datum $depth (union-constructor-rhs $union-constructor))))
      ((union-eliminator? $union-eliminator)
        `(union-case
          ,(term->datum $depth (union-eliminator-lhs $union-eliminator))
          ,@(terms->datum $depth (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        `(
          ,(primitive-application-symbol $primitive-application)
          ,@(map
            (partial term->datum $depth)
            (primitive-application-args $primitive-application))))))

  (define (subst->datum $subst)
    `(subst
      ,@(map
        (lambda ($obj)
          (switch $obj
            ((blank? _) 'blank)
            ((else $term) (term->datum 0 $term))))
        $subst)))

  (define (variable->syntax $variable)
    (literal->syntax
      (string->symbol
        (string-append "$"
          (number->string (variable-index $variable))))))

  (define (terms->syntax $depth $terms)
    #`(list
      #,@(map
        (partial term->syntax $depth)
        $terms)))

  (define (term->syntax $depth $term)
    (term-switch $term
      ((constant? $constant)
        (literal->syntax $constant))
      ((kind? $kind)
        #`(kind #,(literal->syntax (kind-index $kind))))
      ((variable? $variable)
        (variable->syntax $variable))
      ((abstraction? $abstraction)
        (lets
          ($variable (variable $depth))
          #`(abstraction
            (lambda (#,(variable->syntax $variable))
              #,(term->syntax
                (+ $depth 1)
                (abstraction-apply $abstraction $variable))))))
      ((pi? $pi)
        (lets
          ($variable (variable $depth))
          #`(pi
            #,(term->syntax  $depth (pi-domain $pi))
            (lambda (#,(variable->syntax $variable))
              #,(term->syntax
                (+ $depth 1)
                (pi-apply $pi $variable))))))
      ((application? $application)
        #`(application
          #,(term->syntax $depth (application-lhs $application))
          #,(term->syntax $depth (application-rhs $application))))
      ((hole? $hole)
        #`(hole
          #,(literal->syntax (hole-index $hole))
          #,(term->syntax $depth (hole-domain $hole))))
      ((type-constructor? $type-constructor)
        #`(type-constructor
          '#,(literal->syntax (type-constructor-symbol $type-constructor))
          #,(terms->syntax $depth
            (type-constructor-args $type-constructor))))
      ((tuple-constructor? $tuple-constructor)
        #`(tuple-constructor
          #,(terms->syntax $depth
            (tuple-constructor-args $tuple-constructor))))
      ((tuple-projection? $tuple-projection)
        #`(tuple-projection
          #,(term->syntax $depth
            (tuple-projection-lhs $tuple-projection))
          #,(literal->syntax
            (tuple-projection-index $tuple-projection))))
      ((union-constructor? $union-constructor)
        #`(union-constructor
          #,(literal->syntax (union-constructor-index $union-constructor))
          #,(term->syntax $depth (union-constructor-rhs $union-constructor))))
      ((union-eliminator? $union-eliminator)
        #`(union-eliminator
          #,(term->syntax $depth (union-eliminator-lhs $union-eliminator))
          #,(terms->syntax $depth (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        #`(primitive-application
          ($primitive 2 #,(literal->syntax (primitive-application-symbol $primitive-application)))
          #,(terms->syntax $depth
            (primitive-application-args $primitive-application))))))

  (define (term=? $depth $lhs $rhs)
    (term-switch $lhs
      ((constant? $lhs)
        (and
          (constant? $rhs)
          (equal? $lhs $rhs)))
      ((kind? $lhs)
        (and
          (kind? $rhs)
          (kind=? $lhs $rhs)))
      ((variable? $lhs)
        (and
          (variable? $rhs)
          (variable=? $lhs $rhs)))
      ((abstraction? $lhs)
        (and
          (abstraction? $rhs)
          (term=? (+ $depth 1)
            (abstraction-apply $lhs (variable $depth))
            (abstraction-apply $rhs (variable $depth)))))
      ((pi? $lhs)
        (and
          (pi? $rhs)
          (term=? $depth
            (pi-domain $lhs)
            (pi-domain $rhs))
          (term=? (+ $depth 1)
            (pi-apply $lhs (variable $depth))
            (pi-apply $rhs (variable $depth)))))
      ((application? $lhs)
        (and
          (application? $rhs)
          (term=? $depth
            (application-lhs $lhs)
            (application-lhs $rhs))
          (term=? $depth
            (application-rhs $lhs)
            (application-rhs $rhs))))
      ((hole? $lhs)
        (and
          (hole? $rhs)
          (and
            (=
              (hole-index $lhs)
              (hole-index $rhs))
            (term=? $depth
              (hole-domain $lhs)
              (hole-domain $rhs)))))
      ((type-constructor? $lhs)
        (and
          (type-constructor? $rhs)
          (symbol=?
            (type-constructor-symbol $lhs)
            (type-constructor-symbol $rhs))
          (for-all* (partial term=? $depth)
            (type-constructor-args $lhs)
            (type-constructor-args $rhs))))
      ((tuple-constructor? $lhs)
        (and
          (tuple-constructor? $rhs)
          (for-all* (partial term=? $depth)
            (tuple-constructor-args $lhs)
            (tuple-constructor-args $rhs))))
      ((tuple-projection? $lhs)
        (and
          (tuple-projection? $rhs)
          (term=? $depth
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
          (term=? $depth
            (union-constructor-rhs $lhs)
            (union-constructor-rhs $rhs))))
      ((union-eliminator? $lhs)
        (and
          (union-eliminator? $rhs)
          (term=? $depth
            (union-eliminator-lhs $lhs)
            (union-eliminator-lhs $rhs))
          (for-all* (partial term=? $depth)
            (union-eliminator-branches $lhs)
            (union-eliminator-branches $rhs))))
      ((primitive-application? $lhs)
        (and
          (primitive-application? $rhs)
          (symbol=?
            (primitive-application-symbol $lhs)
            (primitive-application-symbol $rhs))
          (for-all* (partial term=? $depth)
            (primitive-application-args $lhs)
            (primitive-application-args $rhs))))))


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

  (define (subst-alloc $subst $domain)
    (lets
      ($index (length $subst))
      ($subst (cons blank $subst))
      (values $subst (hole $index $domain))))

  (define (terms-contain-hole? $subst $hole $terms)
    (exists (partial term-contains-hole? $subst $hole) $terms))

  (define (term-contains-hole? $subst $hole $term)
    (term-switch (subst-resolve $subst $term)
      ((constant? _) #f)
      ((kind? _) #f)
      ((variable? _) #f)
      ((abstraction? _) #f)
      ((pi? $pi) (term-contains-hole? $subst $hole (pi-domain $pi)))
      ((application? $app)
        (or
          (term-contains-hole? $subst $hole (application-lhs $app))
          (term-contains-hole? $subst $hole (application-rhs $app))))
      ((hole? $h) (hole-index=? $h $hole))
      ((type-constructor? $tc)
        (terms-contain-hole? $subst $hole (type-constructor-args $tc)))
      ((tuple-constructor? $tc)
        (terms-contain-hole? $subst $hole
          (tuple-constructor-args $tc)))
      ((tuple-projection? $tp)
        (term-contains-hole? $subst $hole (tuple-projection-lhs $tp)))
      ((union-constructor? $uc)
        (term-contains-hole? $subst $hole (union-constructor-rhs $uc)))
      ((union-eliminator? $ue)
        (or
          (term-contains-hole? $subst $hole (union-eliminator-lhs $ue))
          (terms-contain-hole? $subst $hole (union-eliminator-branches $ue))))
      ((primitive-application? $pa)
        (terms-contain-hole? $subst $hole (primitive-application-args $pa)))))

  (define (solve-hole $subst $hole $term)
    (cond
      ((and (hole? $term) (hole-index=? $hole $term)) $subst)
      ((term-contains-hole? $subst $hole $term) #f)
      (else (subst-set $subst $hole $term))))

  (define (terms-unify $depth $subst $lhss $rhss)
    (and
      (= (length $lhss) (length $rhss))
      (fold-left (partial term-unify $depth) $subst $lhss $rhss)))

  (define (term-unify $depth $subst $lhs $rhs)
    (and $subst
      (lets
        ($lhs (subst-resolve $subst $lhs))
        ($rhs (subst-resolve $subst $rhs))
        (cond
          ((and (hole? $lhs) (hole? $rhs))
            (cond
              ((hole-index=? $lhs $rhs) $subst)
              (else (subst-set $subst $lhs $rhs))))

          ((hole? $lhs) (solve-hole $subst $lhs $rhs))
          ((hole? $rhs) (solve-hole $subst $rhs $lhs))

          ((abstraction? $lhs)
            (and
              (abstraction? $rhs)
              (term-unify (+ $depth 1) $subst
                (abstraction-apply $lhs (variable $depth))
                (abstraction-apply $rhs (variable $depth)))))

          ((pi? $lhs)
            (and
              (pi? $rhs)
              (lets
                ($subst
                  (term-unify $depth $subst
                    (pi-domain $lhs)
                    (pi-domain $rhs)))
                (term-unify (+ $depth 1) $subst
                  (pi-apply $lhs (variable $depth))
                  (pi-apply $rhs (variable $depth))))))

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

          ((application? $lhs)
            (and
              (application? $rhs)
              (lets
                ($subst
                  (term-unify $depth $subst
                    (application-lhs $lhs)
                    (application-lhs $rhs)))
                (term-unify $depth $subst
                  (application-rhs $lhs)
                  (application-rhs $rhs)))))

          ((type-constructor? $lhs)
            (and
              (type-constructor? $rhs)
              (symbol=?
                (type-constructor-symbol $lhs)
                (type-constructor-symbol $rhs))
              (terms-unify $depth $subst
                (type-constructor-args $lhs)
                (type-constructor-args $rhs))))

          ((tuple-constructor? $lhs)
            (and
              (tuple-constructor? $rhs)
              (terms-unify $depth $subst
                (tuple-constructor-args $lhs)
                (tuple-constructor-args $rhs))))

          ((tuple-projection? $lhs)
            (and
              (tuple-projection? $rhs)
              (=
                (tuple-projection-index $lhs)
                (tuple-projection-index $rhs))
              (terms-unify $depth $subst
                (tuple-projection-lhs $lhs)
                (tuple-projection-lhs $rhs))))

          ((union-constructor? $lhs)
            (and
              (union-constructor? $rhs)
              (=
                (union-constructor-index $lhs)
                (union-constructor-index $rhs))
              (term-unify $depth $subst
                (union-constructor-rhs $lhs)
                (union-constructor-rhs $rhs))))

          ((union-eliminator? $lhs)
            (and
              (union-eliminator? $rhs)
              (lets
                ($subst
                  (term-unify $depth $subst
                    (union-eliminator-lhs $lhs)
                    (union-eliminator-lhs $rhs)))
                (terms-unify $depth $subst
                  (union-eliminator-branches $lhs)
                  (union-eliminator-branches $rhs)))))

          ((primitive-application? $lhs)
            (and
              (primitive-application? $rhs)
              (symbol=?
                (primitive-application-symbol $lhs)
                (primitive-application-symbol $rhs))
              (terms-unify $depth $subst
                (primitive-application-args $lhs)
                (primitive-application-args $rhs))))

          ((constant? $lhs)
            (and
              (constant? $rhs)
              (equal? $lhs $rhs)
              $subst))))))

  (define (term-instantiate $subst $term)
    (lets
      ($term (subst-resolve $subst $term))
      (switch $term
        ((pi? $pi)
          (lets
            ((values $subst $hole) (subst-alloc $subst (pi-domain $pi)))
            (term-instantiate $subst (pi-apply $pi $hole))))
        ((else $other)
          (values $subst $other)))))

  (define (subst-apply* $subst $terms)
    (map (partial subst-apply $subst) $terms))

  (define (subst-apply $subst $term)
    (lets
      ($term (subst-resolve $subst $term))
      (term-switch $term
        ((constant? $constant) $constant)
        ((kind? $kind) $kind)
        ((variable? $variable) $variable)
        ((abstraction? $abstraction)
          (abstraction
            (lambda ($arg)
              (subst-apply $subst
                (abstraction-apply $abstraction $arg)))))
        ((pi? $pi)
          (pi
            (subst-apply $subst
              (pi-domain $pi))
            (lambda ($arg)
              (subst-apply $subst
                (pi-apply $pi $arg)))))
        ((application? $application)
          (application
            (subst-apply $subst
              (application-lhs $application))
            (subst-apply $subst
              (application-rhs $application))))
        ((hole? $hole) $hole)
        ((type-constructor? $type-constructor)
          (type-constructor
            (type-constructor-symbol $type-constructor)
            (subst-apply* $subst
              (type-constructor-args $type-constructor))))
        ((tuple-constructor? $tuple-constructor)
          (tuple-constructor
            (subst-apply* $subst
              (tuple-constructor-args $tuple-constructor))))
        ((tuple-projection? $tuple-projection)
          (tuple-projection
            (subst-apply $subst
              (tuple-projection-lhs $tuple-projection))
            (tuple-projection-index $tuple-projection)))
        ((union-constructor? $union-constructor)
          (union-constructor
            (union-constructor-index $union-constructor)
            (subst-apply $subst
              (union-constructor-rhs $union-constructor))))
        ((union-eliminator? $union-eliminator)
          (union-eliminator
            (subst-apply $subst
              (union-eliminator-lhs $union-eliminator))
            (subst-apply* $subst
              (union-eliminator-branches $union-eliminator))))
        ((primitive-application? $primitive-application)
          (primitive-application
            (primitive-application-symbol $primitive-application)
            (subst-apply* $subst
              (primitive-application-args $primitive-application)))))))

  (define (terms-replace $replaced-hole $replacement-term $terms)
    (map
      (partial term-replace $replaced-hole $replacement-term)
      $terms))

  (define (term-replace $replaced-hole $replacement-term $term)
    (term-switch $term
      ((constant? $constant) $constant)
      ((kind? $kind) $kind)
      ((variable? $variable) $variable)
      ((abstraction? $abstraction)
        (abstraction
          (lambda ($arg)
            (term-replace
              $replaced-hole
              $replacement-term
              (abstraction-apply $abstraction $arg)))))
      ((pi? $pi)
        (pi
          (term-replace
            $replaced-hole
            $replacement-term
            (pi-domain $pi))
          (lambda ($arg)
            (term-replace
              $replaced-hole
              $replacement-term
              (pi-apply $pi $arg)))))
      ((application? $application)
        (application
          (term-replace
            $replaced-hole
            $replacement-term
            (application-lhs $application))
          (term-replace
            $replaced-hole
            $replacement-term
            (application-rhs $application))))
      ((hole? $hole)
        (cond
          ((hole-index=? $hole $replaced-hole) $replacement-term)
          (else $hole)))
      ((type-constructor? $type-constructor)
        (type-constructor
          (type-constructor-symbol $type-constructor)
          (terms-replace $replaced-hole $replacement-term
            (type-constructor-args $type-constructor))))
      ((tuple-constructor? $tuple-constructor)
        (tuple-constructor
          (terms-replace $replaced-hole $replacement-term
            (tuple-constructor-args $tuple-constructor))))
      ((tuple-projection? $tuple-projection)
        (tuple-projection
          (term-replace $replaced-hole $replacement-term
            (tuple-projection-lhs $tuple-projection))
          (tuple-projection-index $tuple-projection)))
      ((union-constructor? $union-constructor)
        (union-constructor
          (union-constructor-index $union-constructor)
          (term-replace $replaced-hole $replacement-term
            (union-constructor-rhs $union-constructor))))
      ((union-eliminator? $union-eliminator)
        (union-eliminator
          (term-replace $replaced-hole $replacement-term
            (union-eliminator-lhs $union-eliminator))
          (terms-replace $replaced-hole $replacement-term
            (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        (primitive-application
          (primitive-application-symbol $primitive-application)
          (terms-replace $replaced-hole $replacement-term
            (primitive-application-args $primitive-application))))))

  (define (append-terms-holes $depth $holes $terms)
    (fold-left
      (partial append-term-holes $depth)
      $holes $terms))

  (define (append-term-holes $depth $holes $term)
    (term-switch $term
      ((constant? _) $holes)
      ((kind? _) $holes)
      ((variable? _) $holes)
      ((abstraction? $abstraction)
        (append-term-holes (+ $depth 1) $holes
          (abstraction-apply $abstraction (variable $depth))))
      ((pi? $pi)
        (lets
          ($holes
            (append-term-holes $depth $holes
              (pi-domain $pi)))
          (append-term-holes (+ $depth 1) $holes
            (pi-apply $pi (variable $depth)))))
      ((application? $application)
        (lets
          ($holes
            (append-term-holes $depth $holes
              (application-lhs $application)))
          (append-term-holes $depth $holes
            (application-rhs $application))))
      ((hole? $hole)
        (cons/nodup hole-index=? $hole $holes))
      ((type-constructor? $type-constructor)
        (append-terms-holes $depth
          $holes
          (type-constructor-args $type-constructor)))
      ((tuple-constructor? $tuple-constructor)
        (append-terms-holes $depth
          $holes
          (tuple-constructor-args $tuple-constructor)))
      ((tuple-projection? $tuple-projection)
        (append-term-holes $depth
          $holes
          (tuple-projection-lhs $tuple-projection)))
      ((union-constructor? $union-constructor)
        (append-term-holes $depth
          $holes
          (union-constructor-rhs $union-constructor)))
      ((union-eliminator? $union-eliminator)
        (lets
          ($holes
            (append-term-holes $depth $holes
              (union-eliminator-lhs $union-eliminator)))
          (append-terms-holes $depth $holes
            (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        (append-terms-holes $depth
          $holes
          (primitive-application-args $primitive-application)))))

  (define (term-generalize $hole $term)
    (pi
      (hole-domain $hole)
      (lambda ($arg)
        (term-replace $hole $arg $term))))

  (define (term-generalize* $holes $term)
    (fold-left
      (lambda ($term $hole)
        (term-generalize $hole $term))
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
    ((pi* body) body)
    ((pi* (id t) params ... body)
      (pi t
        (lambda (id)
          (pi* params ... body)))))

  (define (arity-term $arity $procedure)
    (lets
      ($indices (iota $arity))
      ($holes (map hole $indices))
      (term-generalize* $holes ($procedure $holes))))

  (define (term-finalize $subst $term)
    (lets
      ($term (subst-apply $subst $term))
      ($holes (append-term-holes 0 (list) $term))
      (term-generalize* (reverse $holes) $term)))
)
