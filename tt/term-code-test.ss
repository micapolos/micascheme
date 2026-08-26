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
    '(pi-value "string" (lambda ($0) $0))))

(check
  (equal?
    (term->code 0 (pi "string" (pi "number" (variable 0))))
    '(pi-value "string" (lambda ($0) (pi-value "number" (lambda ($1) $1))))))

(check
  (equal?
    (term->code 0 (pi "string" (pi "number" (variable 1))))
    '(pi-value "string" (lambda ($0) (pi-value "number" (lambda ($1) $0))))))

(check
  (equal?
    (term->code 0 (application "foo" "bar"))
    '(term-apply "foo" "bar")))

(check
  (equal?
    (term->code 0 (application "foo" "bar"))
    '(term-apply "foo" "bar")))

