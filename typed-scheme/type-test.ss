(import (micascheme) (typed-scheme type))

(define type-definition-1 (type-definition #f (gensym) "type-1" (list)))
(define type-definition-2 (type-definition #f (gensym) "type-2" (list)))
(define type-definition-3 (type-definition #f (gensym) "type-3" (list)))
(define type-definition-4 (type-definition #f (gensym) "type-4" (list)))

(define type-definition-1-1 (type-definition type-definition-1 (gensym) "type-1-1" (list)))
(define type-definition-1-2 (type-definition type-definition-1 (gensym) "type-1-2" (list)))

(define type-1 (defined-type #f type-definition-1 (list)))
(define type-2 (defined-type #f type-definition-2 (list)))
(define type-3 (defined-type #f type-definition-3 (list)))
(define type-4 (defined-type #f type-definition-4 (list)))

(define type-1-1 (defined-type type-1 type-definition-1-1 (list)))
(define type-1-2 (defined-type type-1 type-definition-1-2 (list)))

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
    (lambda-type (list type-1) type-2)
    (lambda-type (list type-1) type-2)))

(check
  (not
    (type-assignable-to?
      (lambda-type (list type-1) type-2)
      (lambda-type (list type-2) type-1))))

(check (type-assignable-to? type-1 (union-type (list type-1 type-2))))
(check (type-assignable-to? type-2 (union-type (list type-1 type-2))))
(check (type-assignable-to? type-1-1 (union-type (list type-1 type-2))))
(check (type-assignable-to? type-1-2 (union-type (list type-1 type-2))))
(check (not (type-assignable-to? type-3 (union-type (list type-1 type-2)))))

(check
  (type-assignable-to?
    (union-type (list type-1 type-2))
    (union-type (list type-1 type-2))))

(check
  (type-assignable-to?
    (union-type (list type-1 type-2))
    (union-type (list type-1 type-2 type-3))))

(check
  (not
    (type-assignable-to?
      (union-type (list type-1 type-2))
      type-1)))

(check
  (not
    (type-assignable-to?
      (union-type (list type-1 type-2))
      type-2)))

(check
  (not
    (type-assignable-to?
      (union-type (list type-1 type-2))
      (union-type (list type-1 type-3)))))

(check (type-assignable-to? (intersection-type (list type-1 type-2)) type-1))
(check (type-assignable-to? (intersection-type (list type-1 type-2)) type-2))
(check (type-assignable-to? (intersection-type (list type-1-1 type-2)) type-1))
(check (type-assignable-to? (intersection-type (list type-1-2 type-2)) type-1))

(check
  (type-assignable-to?
    (intersection-type (list type-1 type-2))
    (intersection-type (list type-1 type-2))))

(check
  (type-assignable-to?
    (intersection-type (list type-1 type-2 type-3))
    (intersection-type (list type-1 type-2))))

(check
  (not
    (type-assignable-to?
      (intersection-type (list type-1))
      (intersection-type (list type-1 type-2)))))

(check
  (type-assignable-to?
    type-1
    (intersection-type (list type-1 type-1))))

(check
  (not
    (type-assignable-to?
      type-1
      (intersection-type (list type-1 type-2)))))

(check
  (type-assignable-to?
    (forall-type (list in-variance) type-1)
    (forall-type (list in-variance) type-1)))

(check
  (not
    (type-assignable-to?
      (forall-type (list in-variance) type-1)
      (forall-type (list in-variance) type-2))))

(check
  (not
    (type-assignable-to?
      (forall-type (list in-variance) type-1)
      (forall-type (list out-variance) type-1))))

;(check (type-assignable-to? type-1 (forall-type 3 type-1)))

; === type+

(check
  (equal?
    (type+ type-1 type-1)
    type-1))

(check
  (equal?
    (type+ type-1 type-2)
    (union-type (list type-1 type-2))))

(check
  (equal?
    (type+
      (union-type (list type-1 type-2))
      (union-type (list type-2 type-3)))
    (union-type (list type-1 type-2 type-3))))

(check
  (equal?
    (type+
      (union-type (list))
      (union-type (list)))
    (union-type (list))))

; --- type-substitute ---

(define scope-type-substitute
  (partial proc-scope-type-substitute
    (lambda ($scope $value)
      (box $value))))

(check
  (equal?
    (scope-type-substitute
      (stack type-1 #f type-2)
      (variable-type 3))
    (variable-type 3)))

(check
  (equal?
    (scope-type-substitute
      (stack type-1 #f type-2)
      (variable-type 2))
    type-1))

(check
  (equal?
    (scope-type-substitute
      (stack type-1 #f type-2)
      (variable-type 1))
    (variable-type 1)))

(check
  (equal?
    (scope-type-substitute
      (stack type-1 #f type-2)
      (variable-type 0))
    type-2))

(define type-substitute (partial scope-type-substitute (stack)))

(check
  (equal?
    (type-substitute (native-type 'foo))
    (native-type (box 'foo))))

(check
  (equal?
    (scope-type-substitute
      (stack type-1 #f type-2)
      (lambda-type (list (variable-type 0) (variable-type 1)) (variable-type 2)))
    (lambda-type (list type-2 (variable-type 1)) type-1)))
