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

    hole
    hole?
    hole-index
    hole=?

    arrow
    arrow?
    arrow-lhs
    arrow-rhs

    term?
    term-switch

    term=?
    term->datum
    term-apply
    unify
    subst-resolve
    subst-apply
    instantiate
    append-term-holes
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
  (data (hole index))
  (data (abstraction procedure))
  (data (application lhs rhs))
  (data (arrow lhs rhs))

  (union (term universe native hole abstraction application arrow))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

  (define (term-apply $lhs $rhs)
    (switch $lhs
      ((abstraction? $lhs)
        (abstraction-apply $lhs $rhs))
      ((else $lhs)
        (application $lhs $rhs))))

  (define (hole=? $lhs $rhs)
    (=
      (hole-index $lhs)
      (hole-index $rhs)))

  (define (term->datum $obj->datum $depth $term)
    (term-switch $term
      ((universe? $universe)
        (string->symbol
          (apply string-append
            (intercalate
              (map (always "type") (iota (+ (universe-depth $universe) 1)))
              "-"))))
      ((native? $native)
        ($obj->datum $depth (native-ref $native)))
      ((hole? $hole)
        (string->symbol
          (string-append "v"
            (number->string (hole-index $hole)))))
      ((abstraction? $abstraction)
        (lets
          ($hole (hole $depth))
          `(lambda
            ,(term->datum $obj->datum $depth $hole)
            ,(term->datum $obj->datum (+ $depth 1)
              (abstraction-apply $abstraction $hole)))))
      ((application? $application)
        `(
          ,(term->datum $obj->datum $depth (application-lhs $application))
          ,(term->datum $obj->datum $depth (application-rhs $application))))
      ((arrow? $arrow)
        `(arrow
          ,(term->datum $obj->datum $depth (arrow-lhs $arrow))
          ,(term->datum $obj->datum $depth (arrow-rhs $arrow))))))

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
      ((hole? $lhs)
        (and
          (hole? $rhs)
          (hole=? $lhs $rhs)))
      ((abstraction? $lhs)
        (and
          (abstraction? $rhs)
          (term=? $obj=? (+ $index 1)
            (abstraction-apply $lhs (hole $index))
            (abstraction-apply $rhs (hole $index)))))
      ((application? $lhs)
        (and
          (application? $rhs)
          (term=? $obj=? $index
            (application-lhs $lhs)
            (application-lhs $rhs))
          (term=? $obj=? $index
            (application-rhs $lhs)
            (application-rhs $rhs))))
      ((arrow? $arrow)
        (and
          (arrow? $rhs)
          (term=? $obj=? $index
            (arrow-lhs $lhs)
            (arrow-lhs $rhs))
          (term=? $obj=? $index
            (arrow-rhs $lhs)
            (arrow-rhs $rhs))))))

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
          ((false? _) $term)
          ((else $term) (subst-resolve $subst $term))))
      ((else $term) $term)))

  (define (subst-alloc $subst)
    (lets
      ($index (length $subst))
      ($subst (cons #f $subst))
      (values $subst (make-hole $index))))

  (define (unify $native-unify $subst $lhs $rhs)
    (lets
      ($lhs (subst-resolve $subst $lhs))
      ($rhs (subst-resolve $subst $rhs))
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
            (unify $native-unify $subst (abstraction-apply $lhs $hole) $rhs)))

        ((abstraction? $rhs)
          (lets
            ((values $subst $hole) (subst-alloc $subst))
            (unify $native-unify $subst $lhs (abstraction-apply $rhs $hole))))

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

        ((and (arrow? $lhs) (arrow? $rhs))
          (lets?
            ($subst
              (unify $native-unify $subst
                (arrow-lhs $lhs)
                (arrow-lhs $rhs)))
            (unify $native-unify $subst
                (arrow-rhs $lhs)
                (arrow-rhs $rhs))))

        (else #f))))

  (define (instantiate $subst $term)
    (lets
      ($term (subst-resolve $subst $term))
      (cond
        ((abstraction? $term)
          (lets
            ((values $subst $hole) (subst-alloc $subst))
            (instantiate $subst (abstraction-apply $term $hole))))
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
        ((hole? $hole)
          $hole)
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
              (application-rhs $application))))
        ((arrow? $arrow)
          (arrow
            (subst-apply $native-apply $subst
              (arrow-lhs $arrow))
            (subst-apply $native-apply $subst
              (arrow-rhs $arrow)))))))

  (define (term-replace $obj-replace $term $replaced-hole $replacement-term)
    (term-switch $term
      ((universe? $universe)
        $universe)
      ((native? $native)
        ($obj-replace
          (native-ref $native)
          $replaced-hole
          $replacement-term))
      ((hole? $hole)
        (cond
          ((hole=? $hole $replaced-hole) $replacement-term)
          (else $hole)))
      ((abstraction? $abstraction)
        (abstraction
          (lambda ($arg)
            (term-replace
              $obj-replace
              (abstraction-apply $abstraction $arg)
              $replaced-hole
              $replacement-term))))
      ((application? $application)
        (application
          (term-replace $obj-replace
            (application-lhs $application)
            $replaced-hole
            $replacement-term)
          (term-replace $obj-replace
            (application-rhs $application)
            $replaced-hole
            $replacement-term)))
      ((arrow? $arrow)
        (arrow
          (term-replace $obj-replace
            (arrow-lhs $arrow)
            $replaced-hole
            $replacement-term)
          (term-replace $obj-replace
            (arrow-rhs $arrow)
            $replaced-hole
            $replacement-term)))))

  (define (append-term-holes $append-obj-holes $depth $holes $term)
    (term-switch $term
      ((universe? $universe)
        $holes)
      ((native? $native)
        ($append-obj-holes $depth $holes (native-ref $native)))
      ((hole? $hole)
        (cond
          ((>= (hole-index $hole) $depth) $holes)
          (else (cons/nodup hole=? $hole $holes))))
      ((abstraction? $abstraction)
        (append-term-holes $append-obj-holes $depth $holes
          (abstraction-apply $abstraction (hole $depth))))
      ((application? $application)
        (lets
          ($holes
            (append-term-holes $append-obj-holes $depth $holes
              (application-lhs $application)))
          (append-term-holes $append-obj-holes $depth $holes
            (application-rhs $application))))
      ((arrow? $arrow)
        (lets
          ($holes
            (append-term-holes $append-obj-holes $depth $holes
              (arrow-lhs $arrow)))
          (append-term-holes $append-obj-holes $depth $holes
            (arrow-rhs $arrow))))))

  (define (term-generalize $native-replace $term $hole)
    (abstraction
      (lambda ($arg)
        (term-replace $native-replace $term $hole $arg))))
)
