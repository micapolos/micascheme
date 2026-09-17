(library (tt term-code)
  (export
    term->code)
  (import
    (scheme)
    (data)
    (lets)
    (procedure)
    (indexed)
    (tt term))

  (data (lambda-type domain procedure))
  (data (type symbol args))
  (data (neutral lhs rhs))

  (union (value constant kind neutral procedure lambda-type vector indexed type))

  (define (value-ground? $value)
    (value-switch $value
      ((constant? _) #t)
      ((kind? _) #t)
      ((procedure? _) #t)
      ((lambda-type? $lt) (value-ground? (lambda-type-domain $lt)))
      ((vector? $vec) (for-all value-ground? (vector->list $vec)))
      ((indexed? $ind) (value-ground? (indexed-value $ind)))
      ((type? $t) (for-all ground? (type-args $t)))))

  (define (term-apply $lhs $rhs)
    (cond
      ((procedure? $lhs) ($lhs $rhs))
      ((lambda-type? $lhs) ((lambda-type-procedure $lhs) $rhs))
      (else (neutral $lhs $rhs))))

  (define (primitive-apply $symbol $primitive $args)
    (cond
      ((for-all ground? $args)
        (apply $primitive $args))
      (else
        (primitive-application $symbol $args))))

  (define (tuple $args)
    (cond
      ((for-all ground? $args) (apply vector $args))
      (else (tuple-constructor $args))))

  (define (tuple-ref $lhs $index)
    (cond
      ((ground? $lhs) (vector-ref $lhs $index))
      (else (tuple-projection $lhs $index))))

  (define (union $index $rhs)
    (cond
      ((ground? $rhs) (indexed $rhs $index))
      (else (union-constructor $index $rhs))))

  (define (union-case $lhs $branches)
    (cond
      ((ground? $lhs)
        (lets
          ($branch (list-ref $branches (indexed-index $lhs)))
          (cond
            ((ground? $branch) ($branch (indexed-value $lhs)))
            (else (union-eliminator $lhs $branches)))))
      (else
        (union-eliminator $lhs $branches))))

  (define (depth->index $depth)
    (string->symbol (string-append "$" (number->string $depth))))

  (define (term-list->code $depth $terms)
    `(list ,@(terms->code $depth $terms)))

  (define (terms->code $depth $terms)
    (map (partial term->code $depth) $terms))

  (define (term->code $depth $term)
    (term-switch $term
      ((constant? $const)
        $const)
      ((kind? $kind)
        `(kind ,(kind-index $kind)))
      ((variable? $var)
        (depth->index (- $depth (variable-index $var) 1)))
      ((abstraction? $abs)
        `(lambda (,(depth->index $depth))
          ,(term->code (+ $depth 1) (abstraction-body $abs))))
      ((pi? $pi)
        `(lambda-type
          ,(term->code $depth (pi-domain $pi))
          (lambda (,(depth->index $depth))
            ,(term->code (+ $depth 1) (pi-body $pi)))))
      ((application? $app)
        `(term-apply
          ,(term->code $depth (application-lhs $app))
          ,(term->code $depth (application-rhs $app))))
      ((hole? $hole)
        `(hole
          ,(hole-index $hole)
          ,(term->code $depth (hole-domain $hole))
          ,(hole-depth $hole)))
      ((type-constructor? $tc)
        `(type
          ',(type-constructor-symbol $tc)
          ,(term-list->code $depth (type-constructor-args $tc))))
      ((tuple-constructor? $tc)
        `(tuple
          ,(term-list->code $depth (tuple-constructor-args $tc))))
      ((tuple-projection? $tp)
        `(tuple-ref
          ,(term->code $depth (tuple-projection-lhs $tp))
          ,(tuple-projection-index $tp)))
      ((union-constructor? $uc)
        `(union
          ,(union-constructor-index $uc)
          ,(term->code $depth (union-constructor-rhs $uc))))
      ((union-eliminator? $ue)
        `(union-case
          ,(term->code $depth (union-eliminator-lhs $ue))
          ,(term-list->code $depth (union-eliminator-branches $ue))))
      ((primitive-application? $pa)
        `(primitive-apply
          ',(primitive-application-symbol $pa)
          ($primitive ,(primitive-application-symbol $pa))
          ,(term-list->code $depth (primitive-application-args $pa))))))
)
