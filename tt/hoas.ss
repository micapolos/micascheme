(library (tt hoas)
  (export
    universe
    universe?
    universe-depth

    native
    native?
    native-ref

    abstraction
    abstraction?
    abstraction-procedure
    abstraction-apply

    application
    application?
    application-lhs
    application-rhs

    variable
    variable?
    variable-index
    variable=?

    term?
    term-switch

    term=?
    term->datum
    term-apply
    unify
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

  (data (universe depth))
  (data (native ref))
  (data (variable index))
  (data (abstraction procedure))
  (data (application lhs rhs))

  (union (term universe native variable abstraction application))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

  (define (term-apply $lhs $rhs)
    (switch $lhs
      ((abstraction? $lhs)
        (abstraction-apply $lhs $rhs))
      ((else $lhs)
        (application $lhs $rhs))))

  (define (variable=? $lhs $rhs)
    (=
      (variable-index $lhs)
      (variable-index $rhs)))

  (define (term->datum $obj->datum $depth $term)
    (term-switch $term
      ((universe? $universe)
        (string->symbol
          (apply string-append
            (intercalate
              (map (always "type") (iota (universe-depth $universe)))
              "-"))))
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
              (abstraction-apply $abstraction $variable)))))
      ((application? $application)
        `(
          ,(term->datum $obj->datum $depth (application-lhs $application))
          ,(term->datum $obj->datum $depth (application-rhs $application))))))

  (define (term=? $obj=? $index $lhs $rhs)
    (term-switch $lhs
      ((universe? $lhs)
        (and
          (universe? $rhs)
          (=
            (universe-depth $lhs)
            (universe-depth $rhs))))
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
            (abstraction-apply $rhs (variable $index)))))
      ((application? $lhs)
        (and
          (term=? $obj=? $index
            (application-lhs $lhs)
            (application-lhs $rhs))
          (term=? $obj=? $index
            (application-rhs $lhs)
            (application-rhs $rhs))))))

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

        ((and (application? $lhs) (application? $rhs))
          (lets?
            ($subst
              (unify $native-unify $subst
                (application-lhs $lhs)
                (application-lhs $rhs)))
            (unify $native-unify $subst
                (application-rhs $lhs)
                (application-rhs $rhs))))

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
        ((universe? $universe)
          $universe)
        ((native? $native)
          ($native-apply $subst (native-ref $native)))
        ((variable? $variable)
          $variable)
        ((abstraction? $abstraction)
          (abstraction
            (lambda ($arg)
              (subst-apply $native-apply $subst
                (abstraction-apply $abstraction $arg)))))
        ((application? $application)
          (application
            (subst-apply $native-apply $subst
              (application-lhs $application))
            (subst-apply $native-apply $subst
              (application-rhs $application)))))))

  (define (term-replace $obj-replace $term $replaced-variable $replacement-term)
    (term-switch $term
      ((universe? $universe)
        $universe)
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
              $replacement-term))))
      ((application? $application)
        (application
          (term-replace $obj-replace
            (application-lhs $application)
            $replaced-variable
            $replacement-term)
          (term-replace $obj-replace
            (application-rhs $application)
            $replaced-variable
            $replacement-term)))))

  (define (append-term-variables $append-obj-variables $depth $variables $term)
    (term-switch $term
      ((universe? $universe)
        $variables)
      ((native? $native)
        ($append-obj-variables $depth $variables (native-ref $native)))
      ((variable? $variable)
        (cond
          ((>= (variable-index $variable) $depth) $variables)
          (else (cons/nodup variable=? $variable $variables))))
      ((abstraction? $abstraction)
        (append-term-variables $append-obj-variables $depth $variables
          (abstraction-apply $abstraction (variable $depth))))
      ((application? $application)
        (lets
          ($variables
            (append-term-variables $append-obj-variables $depth $variables
              (application-lhs $application)))
          (append-term-variables $append-obj-variables $depth $variables
            (application-rhs $application))))))

  (define (term-generalize $native-replace $term $variable)
    (abstraction
      (lambda ($arg)
        (term-replace $native-replace $term $variable $arg))))
)
