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

  (define (term-ground? $term)
    (or
      (constant? $term)
      (kind? $term)
      (procedure? $term)
      (lambda-type? $term)
      (pair? $term)))

  (define (term-apply $lhs $rhs)
    (cond
      ((procedure? $lhs) ($lhs $rhs))
      ((lambda-type? $lhs) ((lambda-type-procedure $lhs) $rhs))
      (else (application $lhs $rhs))))

  (define (primitive-apply $symbol $primitive $args)
    (cond
      ((for-all term-ground? $args)
        (apply $primitive $args))
      (else
        (primitive-application $symbol $args))))

  (define (tuple $args)
    (cond
      ((for-all term-ground? $args) (apply vector $args))
      (else (tuple-constructor $args))))

  (define (tuple-ref $lhs $index)
    (cond
      ((term-ground? $lhs) (vector-ref $lhs $index))
      (else (tuple-projection $lhs $index))))

  (define (union $index $rhs)
    (cond
      ((term-ground? $rhs) (indexed $rhs $index))
      (else (union-constructor $index $rhs))))

  (define (union-case $lhs $branches)
    (cond
      ((term-ground? $lhs)
        (lets
          ($branch (list-ref $branches (indexed-index $lhs)))
          (cond
            ((term-ground? $branch) ($branch (indexed-value $lhs)))
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
