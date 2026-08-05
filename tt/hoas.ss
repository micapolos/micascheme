(library (tt hoas)
  (export
    variable
    variable?
    variable-index
    variable=?

    abstraction
    abstraction?
    abstraction-procedure
    abstraction-apply
    abstraction*

    application
    application?
    application-lhs
    application-rhs
    application*

    hole
    hole?
    hole-index
    hole=?

    unified
    unified?
    unified-subst
    unified-ref
    unified-map

    term?
    term-switch

    index->datum
    term-dynamic?
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
    term-intersect?
    term-finalize
    arity-term

    native-abstraction)
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
    (prefix (tt keywords) %))

  (data (variable index))
  (data (abstraction procedure))
  (data (application lhs rhs))
  (data (hole index))

  (union (term variable abstraction application hole))
  (data (unified subst ref))

  (define (unified-map $fn $unified)
    (unified
      (unified-subst $unified)
      ($fn (unified-ref $unified))))

  (define (variable=? $lhs $rhs)
    (=
      (variable-index $lhs)
      (variable-index $rhs)))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

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

  (define (fold-term-params $params $depth $term)
    (switch $term
      ((abstraction? $abstraction)
        (lets
          ($variable (variable $depth))
          (fold-term-params
            (cons (variable->datum $variable) $params)
            (+ $depth 1)
            (abstraction-apply $abstraction $variable))))
      ((else _) $params)))

  (define (term->params $depth $term)
    (reverse (fold-term-params (list) $depth $term)))

  (define (abstraction-body->datum $obj->datum $depth $term)
    (switch $term
      ((abstraction? $abstraction)
        (abstraction-body->datum
          $obj->datum
          (+ $depth 1)
          (abstraction-apply $abstraction (variable $depth))))
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
      ((variable? $variable)
        (variable->datum $variable))
      ((abstraction? $abstraction)
        `(forall
          ,(term->params $depth $abstraction)
          ,(abstraction-body->datum $obj->datum $depth $abstraction)))
      ((application? $application)
        (map
          (partial term->datum $obj->datum $depth)
          (term-arguments $application)))
      ((hole? $hole)
        (hole->datum $hole))
      ((else $obj)
        ($obj->datum $depth $obj))))

  (define (subst->datum $obj->datum $subst)
    `(subst
      ,@(map
        (lambda ($term?)
          (and $term? (term->datum $obj->datum 0 $term?)))
        $subst)))

  (define (variable->syntax $variable)
    (literal->syntax
      (string->symbol
        (string-append "$"
          (number->string (variable-index $variable))))))

  (define (term->syntax $obj->syntax $depth $term)
    (term-switch $term
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
      ((application? $application)
        #`(application
          #,(term->syntax $obj->syntax $depth (application-lhs $application))
          #,(term->syntax $obj->syntax $depth (application-rhs $application))))
      ((hole? $hole)
        #`(hole
          #,(literal->syntax (hole-index $hole))))
      ((else $obj)
        ($obj->syntax $depth $obj))))

  (define (term-dynamic? $obj-dynamic? $depth $term)
    (term-switch $term
      ((variable? _) #t)
      ((abstraction? $abstraction)
        (term-dynamic? $obj-dynamic?
          (+ $depth 1)
          (abstraction-apply $abstraction (variable $depth))))
      ((application? $application)
        (or
          (term-dynamic? $obj-dynamic? $depth (application-lhs $application))
          (term-dynamic? $obj-dynamic? $depth (application-rhs $application))))
      ((hole? _) #t)
      ((else $obj)
        ($obj-dynamic? $depth $obj))))

  (define (term=? $obj=? $index $lhs $rhs)
    (term-switch $lhs
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
      ((else $obj)
        (and
          (not (term? $rhs))
          ($obj=? $index $lhs $rhs)))))

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

  (define (term-unify $obj-unify $subst? $lhs $rhs)
    (lets?
      ($subst $subst?)
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
              (term-unify $obj-unify $subst (abstraction-apply $lhs $hole) $rhs)))

          ((abstraction? $rhs)
            (lets
              ((values $subst $hole) (subst-alloc $subst))
              (term-unify $obj-unify $subst $lhs (abstraction-apply $rhs $hole))))

          ((and (variable? $lhs) (variable? $rhs))
            (variable=? $lhs $rhs))

          ((and (application? $lhs) (application? $rhs))
            (lets?
              ($subst
                (term-unify $obj-unify $subst
                  (application-lhs $lhs)
                  (application-lhs $rhs)))
              (term-unify $obj-unify $subst
                  (application-rhs $lhs)
                  (application-rhs $rhs))))

          ((and (not (term? $lhs)) (not (term? $rhs)))
            ($obj-unify $subst $lhs $rhs))

          (else #f)))))

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
        ((variable? _) $subst)
        ((abstraction? $abstraction)
          (abstraction
            (lambda ($arg)
              (subst-apply $obj-apply $subst
                (abstraction-apply $abstraction $arg)))))
        ((application? $application)
          (application
            (subst-apply $obj-apply $subst
              (application-lhs $application))
            (subst-apply $obj-apply $subst
              (application-rhs $application))))
        ((hole? $hole) $hole)
        ((else $obj)
          ($obj-apply $subst $obj)))))

  (define (term-replace $obj-replace $replaced-hole $replacement-term $term)
    (term-switch $term
      ((variable? $variable) $variable)
      ((abstraction? $abstraction)
        (abstraction
          (lambda ($arg)
            (term-replace
              $obj-replace
              $replaced-hole
              $replacement-term
              (abstraction-apply $abstraction $arg)))))
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
      ((else $obj)
        ($obj-replace $replaced-hole $replacement-term $term))))

  (define (append-term-holes $append-obj-holes $depth $holes $term)
    (term-switch $term
      ((variable? _) $holes)
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
      ((hole? $hole)
        (cond
          ((>= (hole-index $hole) $depth) $holes)
          (else (cons/nodup hole=? $hole $holes))))
      ((else $obj)
        ($append-obj-holes $depth $holes $obj))))

  (define (term-generalize $obj-replace $hole $term)
    (abstraction
      (lambda ($arg)
        (term-replace $obj-replace $hole $arg $term))))

  (define (term-generalize* $obj-replace $holes $term)
    (fold-left
      (lambda ($term $hole) (term-generalize $obj-replace $hole $term))
      $term
      $holes))

  (define (application* $lhs . $rhss)
    (fold-left application $lhs $rhss))

  (define-rules-syntax
    ((abstraction* body) body)
    ((abstraction* param param* ... body)
      (abstraction
        (lambda (param)
          (abstraction* param* ... body)))))

  (define (arity-term $obj-replace $arity $procedure)
    (lets
      ($indices (iota $arity))
      ($holes (map hole $indices))
      ; TODO: This term-generalize* must operate on reversed holes.
      (term-generalize* $obj-replace $holes ($procedure (reverse $holes)))))

  (define-rule-syntax (native-abstraction obj->apply id param ...)
    (abstraction* param ...
      (cond
        ((and (not (term? id)) (not (term? param)) ...)
          (obj->apply id param ...))
        (else
          (application* id param ...)))))

  (define (term-intersect? $obj-unify $append-obj-holes $obj-apply $obj-replace $lhs $rhs)
    (lets
      ((values $subst $lhs) (term-instantiate (list) $lhs))
      ($subst (term-unify $obj-unify $subst $lhs $rhs))
      (and $subst
        (lets
          ($lhs (subst-apply $obj-apply $subst $lhs))
          ($holes (append-term-holes $append-obj-holes 0 (list) $lhs))
          (fold-left
            (lambda ($term $hole) (term-generalize $obj-replace $hole $term))
            $lhs
            $holes)))))

  (define (term-finalize $obj-apply $append-obj-holes $obj-replace $subst $term)
    (lets
      ($term (subst-apply $obj-apply $subst $term))
      ($holes (append-term-holes $append-obj-holes (length $subst) (list) $term))
      (term-generalize* $obj-replace $holes $term)))
)
