(library (tt term)
  (export
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
    abstraction-body

    pi
    pi?
    pi-domain
    pi-body

    application
    application?
    application-lhs
    application-rhs

    hole
    hole?
    hole-index
    hole-domain
    hole-depth
    hole-index=?

    primitive-application
    primitive-application?
    primitive-application-symbol
    primitive-application-args

    type-constructor
    type-constructor?
    type-constructor-symbol
    type-constructor-args

    tuple-constructor
    tuple-constructor?
    tuple-constructor-args

    tuple-projection
    tuple-projection?
    tuple-projection-lhs
    tuple-projection-index

    union-constructor
    union-constructor?
    union-constructor-index
    union-constructor-rhs

    union-eliminator
    union-eliminator?
    union-eliminator-lhs
    union-eliminator-branches

    blank
    blank?

    term?
    term-switch

    term-shift
    terms-shift

    term-valid-in-scope?
    terms-valid-in-scope?

    term-unify
    terms-unify
    subst-resolve
    subst-apply
    subst-apply*
    append-term-holes
    append-terms-holes
    term-replace
    terms-replace)
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
  (data (abstraction body))
  (data (pi domain body))
  (data (application lhs rhs))
  (data (hole index domain depth))
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

  (define (constant? $obj)
    (or
      (boolean? $obj)
      (number? $obj)
      (char? $obj)
      (string? $obj)))

  (define (variable=? $lhs $rhs)
    (=
      (variable-index $lhs)
      (variable-index $rhs)))

  (define (kind=? $lhs $rhs)
    (=
      (kind-index $lhs)
      (kind-index $rhs)))

  (define (hole-index=? $lhs $rhs)
    (=
      (hole-index $lhs)
      (hole-index $rhs)))

  (define (terms-shift $amount $cutoff $terms)
    (map (partial term-shift $amount $cutoff) $terms))

  (define (term-shift $amount $cutoff $term)
    (term-switch $term
      ((constant? $constant) $constant)
      ((kind? $kind) $kind)
      ((variable? $variable)
        (lets
          ($index (variable-index $variable))
          (if (>= $index $cutoff)
            (variable (+ $index $amount))
            $variable)))
      ((abstraction? $abs)
        (abstraction
          (term-shift $amount (+ $cutoff 1)
            (abstraction-body $abs))))
      ((pi? $pi)
        (pi
          (term-shift $amount $cutoff (pi-domain $pi))
           (term-shift $amount (+ $cutoff 1) (pi-body $pi))))
      ((application? $app)
        (application
          (term-shift $amount $cutoff (application-lhs $app))
          (term-shift $amount $cutoff (application-rhs $app))))
      ((hole? $hole) $hole)
      ((type-constructor? $tc)
        (type-constructor
          (type-constructor-symbol $tc)
          (terms-shift $amount $cutoff (type-constructor-args $tc))))
      ((tuple-constructor? $tc)
        (tuple-constructor
          (terms-shift $amount $cutoff (tuple-constructor-args $tc))))
      ((tuple-projection? $tp)
        (tuple-projection
          (term-shift $amount $cutoff (tuple-projection-lhs $tp))
          (tuple-projection-index $tp)))
      ((union-constructor? $uc)
        (union-constructor
          (union-constructor-index $uc)
          (term-shift $amount $cutoff (union-constructor-rhs $uc))))
      ((union-eliminator? $ue)
        (union-eliminator
          (term-shift $amount $cutoff (union-eliminator-lhs $ue))
          (terms-shift $amount $cutoff (union-eliminator-branches $ue))))
      ((primitive-application? $pa)
        (primitive-application
          (primitive-application-symbol $pa)
          (terms-shift $amount $cutoff (primitive-application-args $pa))))))

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

  (define (subst-alloc $depth $subst $domain)
    (lets
      ($index (length $subst))
      ($subst (cons blank $subst))
      (values $subst (hole $index $domain $depth))))

  (define (terms-contain-hole? $subst $hole $terms)
    (exists (partial term-contains-hole? $subst $hole) $terms))

  (define (term-contains-hole? $subst $hole $term)
    (term-switch (subst-resolve $subst $term)
      ((constant? _) #f)
      ((kind? _) #f)
      ((variable? _) #f)
      ((abstraction? $abs)
        (term-contains-hole? $subst $hole (abstraction-body $abs)))
      ((pi? $pi)
        (or
          (term-contains-hole? $subst $hole (pi-domain $pi))
          (term-contains-hole? $subst $hole (pi-body $pi))))
      ((application? $app)
        (or
          (term-contains-hole? $subst $hole(application-lhs $app))
          (term-contains-hole? $subst $hole (application-rhs $app))))
      ((hole? $h)
        (hole-index=? $h $hole))
      ((type-constructor? $tc)
        (terms-contain-hole? $subst $hole (type-constructor-args $tc)))
      ((tuple-constructor? $tc)
        (terms-contain-hole? $subst $hole (tuple-constructor-args $tc)))
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

  (define (solve-hole $depth $subst $hole $term)
    (cond
      ((and (hole? $term) (hole-index=? $hole $term)) $subst)
      ((term-contains-hole? $subst $hole $term) #f)
      ((not (term-valid-in-scope? $depth $subst (hole-depth $hole) $term)) #f)
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
              ;; Always solve the deeper hole in terms of the shallower hole without shifting
              ((<= (hole-depth $lhs) (hole-depth $rhs))
                (solve-hole $depth $subst $rhs $lhs))
              (else
                (solve-hole $depth $subst $lhs $rhs))))

          ((hole? $lhs) (solve-hole $depth $subst $lhs $rhs))
          ((hole? $rhs) (solve-hole $depth $subst $rhs $lhs))

          ((abstraction? $lhs)
            (and
              (abstraction? $rhs)
              (term-unify (+ $depth 1) $subst
                (abstraction-body $lhs)
                (abstraction-body $rhs))))

          ((pi? $lhs)
            (and
              (pi? $rhs)
              (lets
                ($subst
                  (term-unify $depth $subst
                    (pi-domain $lhs)
                    (pi-domain $rhs)))
                (term-unify (+ $depth 1) $subst
                  (pi-body $lhs)
                  (pi-body $rhs)))))

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
              (term-unify $depth $subst
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

  (define (subst-apply* $depth $subst $terms)
    (map (partial subst-apply $depth $subst) $terms))

  (define (subst-apply $depth $subst $term)
    (lets
      ($term (subst-resolve $subst $term))
      (term-switch $term
        ((constant? $constant) $constant)
        ((kind? $kind) $kind)
        ((variable? $variable) $variable)
        ((abstraction? $abstraction)
          (abstraction
            (subst-apply (+ $depth 1) $subst
              (abstraction-body $abstraction))))
        ((pi? $pi)
          (pi
            (subst-apply $depth $subst (pi-domain $pi))
            (subst-apply (+ $depth 1) $subst (pi-body $pi))))
        ((application? $application)
          (application
            (subst-apply $depth $subst
              (application-lhs $application))
            (subst-apply $depth $subst
              (application-rhs $application))))
        ((hole? $hole)
          (lets
            ($resolved (subst-resolve $subst $hole))
            (if (hole? $resolved)
              $resolved
              (subst-apply $depth $subst
                (term-shift (- $depth (hole-depth $hole)) 0 $resolved)))))
        ((type-constructor? $type-constructor)
          (type-constructor
            (type-constructor-symbol $type-constructor)
            (subst-apply* $depth $subst
              (type-constructor-args $type-constructor))))
        ((tuple-constructor? $tuple-constructor)
          (tuple-constructor
            (subst-apply* $depth $subst
              (tuple-constructor-args $tuple-constructor))))
        ((tuple-projection? $tuple-projection)
          (tuple-projection
            (subst-apply $depth $subst
              (tuple-projection-lhs $tuple-projection))
            (tuple-projection-index $tuple-projection)))
        ((union-constructor? $union-constructor)
          (union-constructor
            (union-constructor-index $union-constructor)
            (subst-apply $depth $subst
              (union-constructor-rhs $union-constructor))))
        ((union-eliminator? $union-eliminator)
          (union-eliminator
            (subst-apply $depth $subst
              (union-eliminator-lhs $union-eliminator))
            (subst-apply* $depth $subst
              (union-eliminator-branches $union-eliminator))))
        ((primitive-application? $primitive-application)
          (primitive-application
            (primitive-application-symbol $primitive-application)
            (subst-apply* $depth $subst
              (primitive-application-args $primitive-application)))))))

  (define (terms-replace $hole $replaced-term $terms)
    (map
      (partial term-replace $hole $replaced-term)
      $terms))

  (define (term-replace $hole $replaced-term $term)
    (term-switch $term
      ((constant? $constant) $constant)
      ((kind? $kind) $kind)
      ((variable? $variable) $variable)
      ((abstraction? $abstraction)
        (abstraction
          (term-replace
            $hole
            (term-shift 1 0 $replaced-term)
            (abstraction-body $abstraction))))
      ((pi? $pi)
        (pi
          (term-replace
            $hole
            $replaced-term
            (pi-domain $pi))
          (term-replace
            $hole
            (term-shift 1 0 $replaced-term)
            (pi-body $pi))))
      ((application? $application)
        (application
          (term-replace
            $hole
            $replaced-term
            (application-lhs $application))
          (term-replace
            $hole
            $replaced-term
            (application-rhs $application))))
      ((hole? $hole)
        (cond
          ((hole-index=? $hole $hole) $replaced-term)
          (else $hole)))
      ((type-constructor? $type-constructor)
        (type-constructor
          (type-constructor-symbol $type-constructor)
          (terms-replace $hole $replaced-term
            (type-constructor-args $type-constructor))))
      ((tuple-constructor? $tuple-constructor)
        (tuple-constructor
          (terms-replace $hole $replaced-term
            (tuple-constructor-args $tuple-constructor))))
      ((tuple-projection? $tuple-projection)
        (tuple-projection
          (term-replace $hole $replaced-term
            (tuple-projection-lhs $tuple-projection))
          (tuple-projection-index $tuple-projection)))
      ((union-constructor? $union-constructor)
        (union-constructor
          (union-constructor-index $union-constructor)
          (term-replace $hole $replaced-term
            (union-constructor-rhs $union-constructor))))
      ((union-eliminator? $union-eliminator)
        (union-eliminator
          (term-replace $hole $replaced-term
            (union-eliminator-lhs $union-eliminator))
          (terms-replace $hole $replaced-term
            (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        (primitive-application
          (primitive-application-symbol $primitive-application)
          (terms-replace $hole $replaced-term
            (primitive-application-args $primitive-application))))))

  (define (terms-valid-in-scope? $depth $subst $scope-depth $terms)
    (for-all (partial term-valid-in-scope? $depth $subst $scope-depth) $terms))

  (define (term-valid-in-scope? $depth $subst $scope-depth $term)
    (term-switch (subst-resolve $subst $term)
      ((constant? $constant) #t)
      ((kind? $kind) #t)
      ((variable? $var)
        (lets
          ($index (variable-index $var))
          (or
            (< $index $depth)
            (< (- $index $depth) $scope-depth))))
      ((abstraction? $abs)
        (term-valid-in-scope? (+ $depth 1) $subst $scope-depth
          (abstraction-body $abs)))
      ((pi? $pi)
        (and
          (term-valid-in-scope? $depth $subst $scope-depth
            (pi-domain $pi))
          (term-valid-in-scope?  (+ $depth 1) $subst $scope-depth
            (pi-body $pi))))
      ((application? $app)
        (and
          (term-valid-in-scope? $depth $subst $scope-depth
            (application-lhs $app))
          (term-valid-in-scope? $depth $subst $scope-depth
            (application-rhs $app))))
      ((hole? $hole)
        (<= (hole-depth $hole) $scope-depth))
      ((type-constructor? $type-constructor)
        (terms-valid-in-scope? $depth $subst $scope-depth
          (type-constructor-args $type-constructor)))
      ((tuple-constructor? $tuple-constructor)
        (terms-valid-in-scope? $depth $subst $scope-depth
          (tuple-constructor-args $tuple-constructor)))
      ((tuple-projection? $tuple-projection)
        (term-valid-in-scope? $depth $subst $scope-depth
          (tuple-projection-lhs $tuple-projection)))
      ((union-constructor? $union-constructor)
        (term-valid-in-scope? $depth $subst $scope-depth
          (union-constructor-rhs $union-constructor)))
      ((union-eliminator? $union-eliminator)
        (and
          (term-valid-in-scope? $depth $subst $scope-depth
            (union-eliminator-lhs $union-eliminator))
          (terms-valid-in-scope? $depth $subst $scope-depth
            (union-eliminator-branches $union-eliminator))))
      ((primitive-application? $primitive-application)
        (terms-valid-in-scope? $depth $subst $scope-depth
          (primitive-application-args $primitive-application)))))

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
          (abstraction-body $abstraction)))
      ((pi? $pi)
        (lets
          ($holes
            (append-term-holes $depth $holes
              (pi-domain $pi)))
          (append-term-holes (+ $depth 1) $holes
            (pi-body $pi))))
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
)
