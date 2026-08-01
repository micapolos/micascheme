(library (tt type)
  (export
    type?
    type=?
    type->datum
    type->syntax
    type-unify)
  (import
    (scheme)
    (tt hoas)
    (tt primitive))

  (define (type? $obj)
    (or
      (term? $obj)
      (primitive? $obj)))

  (define (type=? $lhs $rhs)
    (term=? primitive=? 0 $lhs $rhs))

  (define (type->datum $type)
    (term->datum primitive->datum 0 $type))

  (define (type->syntax $type)
    (term->syntax primitive->syntax 0 $type))

  (define (type-unify $type $subst $lhs $rhs)
    (term-unify primitive-unify $subst $lhs $rhs))
)
