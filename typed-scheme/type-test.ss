(import (micascheme) (typed-scheme type))

(define type-1 (defined-type #f (type-definition #f 'type-1 "type-1" 0) (immutable-vector)))
(define type-2 (defined-type #f (type-definition #f 'type-1 "type-2" 0) (immutable-vector)))

(check (type-assignable-to? type-1 type-1))
(check (not (type-assignable-to? type-1 type-2)))
(check (not (type-assignable-to? type-2 type-1)))

(check
  (type-assignable-to?
    (lambda-type (immutable-vector type-1) type-2)
    (lambda-type (immutable-vector type-1) type-2)))

(check
  (not
    (type-assignable-to?
      (lambda-type (immutable-vector type-1) type-2)
      (lambda-type (immutable-vector type-2) type-1))))
