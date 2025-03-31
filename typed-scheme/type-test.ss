(import (micascheme) (typed-scheme type))

(define type-1 (defined-type #f (type-definition #f (gensym) "type-1" 0) (immutable-vector)))
(define type-2 (defined-type #f (type-definition #f (gensym) "type-2" 0) (immutable-vector)))
(define type-3 (defined-type #f (type-definition #f (gensym) "type-3" 0) (immutable-vector)))
(define type-4 (defined-type #f (type-definition #f (gensym) "type-4" 0) (immutable-vector)))

; === type-assignable-to?

(check (type-assignable-to? type-1 type-1))
(check (not (type-assignable-to? type-1 type-2)))
(check (not (type-assignable-to? type-2 type-1)))

(check
  (type-assignable-to?
    (lambda-type 0 (immutable-vector type-1) type-2)
    (lambda-type 0 (immutable-vector type-1) type-2)))

(check
  (not
    (type-assignable-to?
      (lambda-type 0 (immutable-vector type-1) type-2)
      (lambda-type 0 (immutable-vector type-2) type-1))))

(check (type-assignable-to? type-1 (union-type (immutable-vector type-1 type-2))))
(check (type-assignable-to? type-2 (union-type (immutable-vector type-1 type-2))))
(check (not (type-assignable-to? type-3 (union-type (immutable-vector type-1 type-2)))))

(check
  (type-assignable-to?
    (union-type (immutable-vector type-1 type-2))
    (union-type (immutable-vector type-1 type-2))))

(check
  (type-assignable-to?
    (union-type (immutable-vector type-1 type-2))
    (union-type (immutable-vector type-1 type-2 type-3))))

(check
  (not
    (type-assignable-to?
      (union-type (immutable-vector type-1 type-2))
      type-1)))

(check
  (not
    (type-assignable-to?
      (union-type (immutable-vector type-1 type-2))
      type-2)))

(check
  (not
    (type-assignable-to?
      (union-type (immutable-vector type-1 type-2))
      (union-type (immutable-vector type-1 type-3)))))

(check (type-assignable-to? (forall-type 3 type-1) (forall-type 3 type-1)))
(check (not (type-assignable-to? (forall-type 3 type-1) (forall-type 3 type-2))))
(check (not (type-assignable-to? (forall-type 2 type-1) (forall-type 3 type-1))))

; === type+

(check
  (equal?
    (type+ type-1 type-1)
    type-1))

(check
  (equal?
    (type+ type-1 type-2)
    (union-type (immutable-vector type-1 type-2))))

(check
  (equal?
    (type+
      (union-type (immutable-vector type-1 type-2))
      (union-type (immutable-vector type-2 type-3)))
    (union-type (immutable-vector type-1 type-2 type-3))))

(check
  (equal?
    (type+
      (union-type (immutable-vector))
      (union-type (immutable-vector)))
    (union-type (immutable-vector))))
