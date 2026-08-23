(library (tt type)
  (export
    type?
    type=?
    type->datum
    type-subst->datum
    type->syntax
    type-unify
    type-instantiate
    type-subst-apply
    type-replace
    type-holes
    type-generalize
    type-intersect
    type-finalize
    arity-type)
  (import
    (scheme)
    (procedure)
    (tt term)
    (tt primitive))

  (define (type? $obj)
    (or
      (term? $obj)
      (primitive? $obj)))

  (define (type=? $lhs $rhs)
    (term=? primitive=? 0 $lhs $rhs))

  (define (type->datum $type)
    (term->datum primitive->datum 0 $type))

  (define (type-subst->datum $subst)
    (subst->datum primitive->datum $subst))

  (define (type->syntax $type)
    (term->syntax primitive->syntax 0 $type))

  (define (type-unify $subst $lhs $rhs)
    (term-unify primitive-unify $subst $lhs $rhs))

  (define (type-instantiate $type)
    (term-instantiate (list) $type))

  (define (type-subst-apply $subst $type)
    (subst-apply primitive-subst-apply $subst $type))

  (define (type-holes $type)
    (append-term-holes append-primitive-holes 0 (list) $type))

  (define (type-replace $hole $replacement-type $type)
    (term-replace primitive-replace $hole $replacement-type $type))

  (define (type-generalize $hole $type)
    (term-generalize primitive-generalize $hole $type))

  (define arity-type (partial arity-term primitive-replace))

  (define type-intersect
    (partial term-intersect
      primitive-unify
      append-primitive-holes
      primitive-subst-apply
      primitive-replace))

  (define type-finalize
    (partial term-finalize
      primitive-subst-apply
      append-primitive-holes
      primitive-replace))
)
