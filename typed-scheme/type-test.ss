(import (micascheme) (typed-scheme type))

(define type-definition-1 (type-definition #f (gensym) "type-1" (immutable-vector)))
(define type-definition-2 (type-definition #f (gensym) "type-2" (immutable-vector)))
(define type-definition-3 (type-definition #f (gensym) "type-3" (immutable-vector)))
(define type-definition-4 (type-definition #f (gensym) "type-4" (immutable-vector)))

(define type-definition-1-1 (type-definition type-definition-1 (gensym) "type-1-1" (immutable-vector)))
(define type-definition-1-2 (type-definition type-definition-1 (gensym) "type-1-2" (immutable-vector)))

(define type-1 (defined-type #f type-definition-1 (immutable-vector)))
(define type-2 (defined-type #f type-definition-2 (immutable-vector)))
(define type-3 (defined-type #f type-definition-3 (immutable-vector)))
(define type-4 (defined-type #f type-definition-4 (immutable-vector)))

(define type-1-1 (defined-type type-1 type-definition-1-1 (immutable-vector)))
(define type-1-2 (defined-type type-1 type-definition-1-2 (immutable-vector)))

; === variance?

(check (in-variance? in-variance))
(check (out-variance? out-variance))
(check (inout-variance? inout-variance))

(check (not (in-variance? "foo")))
(check (not (out-variance? "foo")))
(check (not (inout-variance? "foo")))

(check (variance? in-variance))
(check (variance? out-variance))
(check (variance? inout-variance))
(check (not (variance? "foo")))

; === type-assignable-to?

(check (type-assignable-to? (native-type 1) (native-type 1)))
(check (not (type-assignable-to? (native-type 1) (native-type 2))))

(check (type-assignable-to? type-1 type-1))
(check (not (type-assignable-to? type-1 type-2)))
(check (not (type-assignable-to? type-2 type-1)))

(check (type-assignable-to? type-1-1 type-1))
(check (type-assignable-to? type-1-2 type-1))
(check (not (type-assignable-to? type-1 type-1-1)))
(check (not (type-assignable-to? type-1 type-1-2)))

(check (type-assignable-to? type-1-1 type-1-1))
(check (type-assignable-to? type-1-2 type-1-2))
(check (not (type-assignable-to? type-1-1 type-1-2)))
(check (not (type-assignable-to? type-1-2 type-1-1)))

(check
  (type-assignable-to?
    (lambda-type (immutable-vector type-1) type-2)
    (lambda-type (immutable-vector type-1) type-2)))

(check
  (not
    (type-assignable-to?
      (lambda-type (immutable-vector type-1) type-2)
      (lambda-type (immutable-vector type-2) type-1))))

(check (type-assignable-to? type-1 (union-type (immutable-vector type-1 type-2))))
(check (type-assignable-to? type-2 (union-type (immutable-vector type-1 type-2))))
(check (type-assignable-to? type-1-1 (union-type (immutable-vector type-1 type-2))))
(check (type-assignable-to? type-1-2 (union-type (immutable-vector type-1 type-2))))
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
;(check (type-assignable-to? type-1 (forall-type 3 type-1)))

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
