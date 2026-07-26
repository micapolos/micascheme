(library (tt hoas)
  (export
    native
    native?
    native-ref

    abstraction
    abstraction?
    abstraction-procedure
    abstraction-apply

    variable
    variable?
    variable-index
    variable=?

    term?
    term-switch

    bind-term
    map-term

    term=?
    term->datum
    unify
    subst->datum
    subst-resolve
    subst-apply
    instantiate
    append-term-variables
    term-replace
    term-generalize)
  (import
    (scheme)
    (procedure)
    (data)
    (lets)
    (list)
    (switch)
    (boolean)
    (union)
    (prefix (tt keywords) %))

  (data (native ref))
  (data (variable index))
  (data (abstraction procedure))

  (union (term native variable abstraction))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

  (define (variable=? $lhs $rhs)
    (=
      (variable-index $lhs)
      (variable-index $rhs)))

  (define (bind-term $fn $term)
    (term-switch $term
      ((native? $native)
        ($fn (native-ref $native)))
      ((variable? $variable)
        $variable)
      ((abstraction? $abstraction)
        (abstraction
          (lambda ($arg)
            (bind-term $fn
              (abstraction-apply $abstraction $arg)))))))

  (define (map-term $fn $term)
    (bind-term
      (lambda ($obj) (native ($fn $obj)))
      $term))

  (define (term=? $obj=? $index $lhs $rhs)
    (term-switch $lhs
      ((native? $lhs)
        (and
          (native? $rhs)
          ($obj=? $index
            (native-ref $lhs)
            (native-ref $rhs))))
      ((variable? $lhs)
        (and
          (variable? $rhs)
          (variable=? $lhs $rhs)))
      ((abstraction? $lhs)
        (and
          (abstraction? $rhs)
          (term=? $obj=? (+ $index 1)
            (abstraction-apply $lhs (variable $index))
            (abstraction-apply $rhs (variable $index)))))))

  (define (term->datum $obj->datum $depth $term)
    (term-switch $term
      ((native? $native)
        ($obj->datum $depth (native-ref $native)))
      ((variable? $variable)
        (string->symbol
          (string-append "v"
            (number->string (variable-index $variable)))))
      ((abstraction? $abstraction)
        (lets
          ($variable (variable $depth))
          `(lambda
            ,(term->datum $obj->datum $depth $variable)
            ,(term->datum $obj->datum (+ $depth 1)
              (abstraction-apply $abstraction $variable)))))))

  (define (subst->datum $term->datum $subst)
    (map
      (lambda ($term?) (and $term? (term->datum $term->datum 0 $term?)))
      $subst))

  (define (subst?->datum $term->datum $subst?)
    (and $subst? (subst->datum $term->datum $subst?)))

  (define (subst-index $subst $variable)
    (- (length $subst) (variable-index $variable) 1))

  (define (subst-ref $subst $variable)
    (list-ref $subst (subst-index $subst $variable)))

  (define (subst-set $subst $variable $type)
    (list-set $subst (subst-index $subst $variable) $type))

  (define (subst-resolve $subst $term)
    (switch $term
      ((variable? $term)
        (switch (subst-ref $subst $term)
          ((false? _) $term)
          ((else $term) (subst-resolve $subst $term))))
      ((else $term) $term)))

  (define (subst-alloc $subst)
    (lets
      ($index (length $subst))
      ($subst (cons #f $subst))
      (values $subst (make-variable $index))))

  (define (unify $native-unify $subst $lhs $rhs)
    (lets
      ($lhs (subst-resolve $subst $lhs))
      ($rhs (subst-resolve $subst $rhs))
      (cond
        ((and (variable? $lhs) (variable? $rhs))
          (cond
            ((= (variable-index $lhs) (variable-index $rhs)) $subst)
            (else (subst-set $subst $lhs $rhs))))

        ((variable? $lhs) (subst-set $subst $lhs $rhs))
        ((variable? $rhs) (subst-set $subst $rhs $lhs))

        ((abstraction? $lhs)
          (lets
            ((values $subst $variable) (subst-alloc $subst))
            (unify $native-unify $subst (abstraction-apply $lhs $variable) $rhs)))

        ((abstraction? $rhs)
          (lets
            ((values $subst $variable) (subst-alloc $subst))
            (unify $native-unify $subst $lhs (abstraction-apply $rhs $variable))))

        ((and (native? $lhs) (native? $rhs))
          ($native-unify
            $subst
            (native-ref $lhs)
            (native-ref $rhs)))

        (else #f))))

  (define (instantiate $subst $term)
    (lets
      ($term (subst-resolve $subst $term))
      (cond
        ((abstraction? $term)
          (lets
            ((values $subst $variable) (subst-alloc $subst))
            (instantiate $subst (abstraction-apply $term $variable))))
        (else
          (values $subst $term)))))

  (define (subst-apply $native-apply $subst $term)
    (lets
      ($term (subst-resolve $subst $term))
      (term-switch $term
        ((native? $native)
          ($native-apply $subst (native-ref $native)))
        ((variable? $variable)
          $variable)
        ((abstraction? $abstraction)
          (abstraction
            (lambda ($arg)
              (subst-apply $native-apply $subst
                (abstraction-apply $abstraction $arg))))))))

  (define (term-replace $obj-replace $term $replaced-variable $replacement-term)
    (term-switch $term
      ((native? $native)
        ($obj-replace
          (native-ref $native)
          $replaced-variable
          $replacement-term))
      ((variable? $variable)
        (cond
          ((variable=? $variable $replaced-variable) $replacement-term)
          (else $variable)))
      ((abstraction? $abstraction)
        (abstraction
          (lambda ($arg)
            (term-replace
              $obj-replace
              (abstraction-apply $abstraction $arg)
              $replaced-variable
              $replacement-term))))))

  (define (append-term-variables $append-obj-variables $depth $variables $term)
    (term-switch $term
      ((native? $native)
        ($append-obj-variables $depth $variables (native-ref $native)))
      ((variable? $variable)
        (cond
          ((>= (variable-index $variable) $depth) $variables)
          (else (cons $variable $variables))))
      ((abstraction? $abstraction)
        (append-term-variables $append-obj-variables $depth $variables
          (abstraction-apply $abstraction (variable $depth))))))

  (define (term-generalize $native-replace $term $variable)
    (abstraction
      (lambda ($arg)
        (term-replace $native-replace $term $variable $arg))))
)
