(import (scheme) (check) (tt term) (tt term-code))

(check
  (equal?
    (term->code 0 "foo")
    '"foo"))

(check
  (equal?
    (term->code 0 (kind 0))
    '(kind 0)))

(check
  (equal?
    (term->code 5 (variable 4))
    '$0))

(check
  (equal?
    (term->code 0 (abstraction (variable 0)))
    '(lambda ($0) $0)))

(check
  (equal?
    (term->code 0 (abstraction (abstraction (variable 0))))
    '(lambda ($0) (lambda ($1) $1))))

(check
  (equal?
    (term->code 0 (abstraction (abstraction (variable 1))))
    '(lambda ($0) (lambda ($1) $0))))

(check
  (equal?
    (term->code 0 (pi "string" (variable 0)))
    '(lambda-type "string" (lambda ($0) $0))))

(check
  (equal?
    (term->code 0 (pi "string" (pi "number" (variable 0))))
    '(lambda-type "string" (lambda ($0) (lambda-type "number" (lambda ($1) $1))))))

(check
  (equal?
    (term->code 0 (pi "string" (pi "number" (variable 1))))
    '(lambda-type "string" (lambda ($0) (lambda-type "number" (lambda ($1) $0))))))

(check
  (equal?
    (term->code 0 (application "foo" "bar"))
    '(term-apply "foo" "bar")))

(check
  (equal?
    (term->code 0 (application "foo" "bar"))
    '(term-apply "foo" "bar")))

(check
  (equal?
    (term->code 0 (tuple-constructor (list "foo" "bar")))
    '(tuple (list "foo" "bar"))))

(check
  (equal?
    (term->code 0 (tuple-projection "tuple" 3))
    '(tuple-ref "tuple" 3)))

(check
  (equal?
    (term->code 0 (union-constructor 3 "three"))
    '(union 3 "three")))

(check
  (equal?
    (term->code 0 (union-eliminator "union" (list "f1" "f2" "f3")))
    '(union-case "union" (list "f1" "f2" "f3"))))

(check
  (equal?
    (term->code 0 (type-constructor 'pair (list "car" "cdr")))
    '(type 'pair (list "car" "cdr"))))

