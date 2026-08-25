(import
  (scheme)
  (check)
  (procedure)
  (switch)
  (data)
  (lets)
  (list)
  (stack)
  (boolean)
  (annotation)
  (keyword)
  (syntax)
  (tt term))

; === term->datum

(define test->datum (partial term->datum 0))

(check
  (equal?
    (test->datum (kind 0))
    '(kind 0)))

(check
  (equal?
    (test->datum "foo")
    '"foo"))

(check
  (equal?
    (test->datum (variable 0))
    '$0))

(check
  (equal?
    (test->datum (hole 0 "number"))
    '(hole 0 "number")))

(check
  (equal?
    (test->datum
      (abstraction (lambda ($arg) $arg)))
    '(forall ($0) $0)))

(check
  (equal?
    (test->datum
      (abstraction
        (lambda ($0)
          (abstraction
            (lambda ($1)
              (application $0 $1))))))
    '(forall ($0 $1) ($0 $1))))

(check
  (equal?
    (test->datum
      (pi
        "foo"
        (lambda ($0)
          (application $0 "bar"))))
    '(pi (($0 "foo")) ($0 "bar"))))

(check
  (equal?
    (test->datum
      (application (variable 0) (variable 1)))
    '($0 $1)))

(check
  (equal?
    (test->datum
      (application (application (variable 0) (variable 1)) (variable 2)))
    '($0 $1 $2)))

(check
  (equal?
    (test->datum
      (application (variable 0) (application (variable 1) (variable 2))))
    '($0 ($1 $2))))

; === term->syntax

(define test->syntax (partial term->syntax 0))

(check
  (equal?
    (syntax->datum (test->syntax (kind 0)))
    '(kind 0)))

(check
  (equal?
    (syntax->datum (test->syntax "foo"))
    '"foo"))

(check
  (equal?
    (syntax->datum
      (test->syntax
        (abstraction
          (lambda ($0)
            (abstraction
              (lambda ($1)
                (application $0 $1)))))))
    '(abstraction (lambda ($0)
      (abstraction (lambda ($1)
        (application $0 $1)))))))

(check
  (equal?
    (syntax->datum
      (test->syntax
        (pi "foo"
          (lambda ($0) $0))))
    '(pi "foo"
      (lambda ($0) $0))))

; === term=?

(define test=? (partial term=? 0))

(check (test=? (kind 0) (kind 0)))
(check (not (test=? (kind 0) (kind 1))))

(check (test=? "foo" "foo"))

(check (not (test=? "foo" "bar")))

(check
  (test=?
    (hole 0 "number")
    (hole 0 "number")))

(check
  (not
    (test=?
      (hole 0 "number")
      (hole 1 "number"))))

(check
  (not
    (test=?
      (hole 0 "number")
      (hole 0 "float"))))

(check
  (test=?
    (abstraction (lambda ($arg) $arg))
    (abstraction (lambda ($arg) $arg))))

(check
  (test=?
    (abstraction
      (lambda ($0)
        (abstraction
          (lambda ($1)
            (application $0 $1)))))
    (abstraction
      (lambda ($0)
        (abstraction
          (lambda ($1)
            (application $0 $1)))))))

(check
  (test=?
    (pi "foo" (lambda ($arg) $arg))
    (pi "foo" (lambda ($arg) $arg))))

; === abstaction* / pi* / application*

(check
  (test=?
    (abstraction* x y (application x y))
    (abstraction
      (lambda (x)
        (abstraction
          (lambda (y)
            (application x y)))))))

(check
  (test=?
    (pi* (x "foo") (y (kind 0)) (application x y))
    (pi "foo"
      (lambda (x)
        (pi (kind 0)
          (lambda (y)
            (application x y)))))))

(check
  (test=?
    (application* "foo" "bar" "goo")
    (application (application "foo" "bar") "goo")))

; --- term-unify

(define test-unify (partial term-unify 0))

(check
  (equal?
    (test-unify (list) 10 10)
    (list)))

(check
  (false?
    (test-unify (list) 10 20)))

(check
  (equal?
    (test-unify (list blank) (hole 0 "number") 10)
    (list 10)))

(check
  (equal?
    (test-unify (list blank) 10 (hole 0 "number"))
    (list 10)))

(check
  (equal?
    (test-unify (list 10) 10 (hole 0 "number"))
    (list 10)))

(check
  (false?
    (test-unify (list 20) 10 (hole 0 "number"))))

(check
  (equal?
    (test-unify (list) (kind 0) (kind 0))
    (list)))

(check
  (false?
    (test-unify (list) (kind 0) (kind 1))))

(check
  (equal?
    (test-unify (list) (variable 0) (variable 0))
    (list)))

(check
  (false?
    (test-unify (list) (variable 0) (variable 1))))

; (check
;   (equal?
;     (test-unify
;       (list)
;       (pi "number" (lambda ($arg) $arg))
;       10)
;     (list 10)))

(check
  (equal?
    (test-unify
      (list)
      (application 10 20)
      (application 10 20))
    (list)))

(check
  (equal?
    (test-unify
      (list blank blank)
      (application (hole 0 "number") (hole 1 "number"))
      (application 10 20))
    (list 20 10)))

; (check
;   (equal?
;     (test-unify
;       (list)
;       (pi "number"
;         (lambda ($0)
;           (pi "number"
;             (lambda ($1)
;               (application $0 $1)))))
;       (application 10 20))
;     (list 20 10)))

(check
  (equal?
    (test-unify
      (list blank)
      (application (hole 0 "number") (hole 0 "number"))
      (application 10 10))
    (list 10)))

(check
  (false?
    (test-unify
      (list blank blank)
      (application (hole 0 "number") (hole 0 "number"))
      (application 10 20))))

; --- term-instantiate

(lets
  ((values $subst $term)
    (term-instantiate
      (list "foo")
      (pi "number"
        (lambda ($0)
          (pi "string"
            (lambda ($1)
              (application $0 $1)))))))
  (run
    (check (equal? $subst (list blank blank "foo")))
    (check (equal? $term (application (hole 1 "number") (hole 2 "string"))))))

; TODO: other cases, implement subst=?

; --- subst-apply

(define test-subst-apply (partial subst-apply))

(check
  (equal?
    (test-subst-apply
      (stack "foo")
      (application 10 (hole 0 "number")))
    (application 10 "foo")))

(check
  (equal?
    (test-subst-apply
      (stack (hole 1 "number") "foo")
      (application 10 (hole 0 "number")))
    (application 10 "foo")))

(check
  (equal?
    (test-subst-apply
      (stack blank "foo")
      (application 10 (hole 0 "number")))
    (application 10 (hole 0 "number"))))

; --- term-replace

(define test-replace (partial term-replace))

(check
  (equal?
    (test-replace
      (hole 1 "string")
      "20"
      (hole 1 "string"))
    "20"))

(check
  (equal?
    (test-replace
      (hole 2 "string")
      "20"
      (hole 1 "string"))
    (hole 1 "string")))

(check
  (equal?
    (test->datum
      (test-replace
        (hole 1 "string")
        "20"
        (abstraction (lambda ($arg) (hole 1 "string")))))
    (test->datum
      (abstraction (lambda ($arg) "20")))))

(check
  (equal?
    (test->datum
      (test-replace
        (hole 1 "string")
        "20"
        (abstraction
          (lambda ($arg)
            (application (hole 0 "string") (hole 1 "string"))))))
    (test->datum
      (abstraction
        (lambda ($arg)
          (application (hole 0 "string") "20"))))))

; --- append-term-holes

(define append-test-holes (partial append-term-holes))

(check
  (equal?
    (append-test-holes 0
      (list (hole 20 "string"))
      (hole 9 "string"))
    (list (hole 9 "string") (hole 20 "string"))))

(check
  (equal?
    (append-test-holes 0
      (list (hole 20 "string"))
      (application (hole 8 "string") (hole 9 "string")))
    (list (hole 9 "string") (hole 8 "string") (hole 20 "string"))))

(check
  (equal?
    (append-test-holes 0
      (list (hole 20 "string"))
      (application (hole 9 "string") (hole 9 "string")))
    (list (hole 9 "string") (hole 20 "string"))))

(check
  (equal?
    (append-test-holes 0
      (list (hole 20 "string"))
      (abstraction (lambda ($arg)
        (application $arg (hole 9 "string")))))
    (list (hole 9 "string") (hole 20 "string"))))

; --- term-generalize

(define test-generalize (partial term-generalize))

(check
  (equal?
    (test->datum
      (test-generalize
        (hole 10 "string")
        (hole 10 "string")))
    '(pi (($0 "string")) $0)))

(check
  (equal?
    (test->datum
      (test-generalize
        (hole 11 "string")
        (hole 10 "string")))
    '(pi (($0 "string")) (hole 10 "string"))))

(check
  (equal?
    (test->datum
      (test-generalize
        (hole 1 "string")
        (application (hole 10 "string") (hole 1 "string"))))
    '(pi (($0 "string")) ((hole 10 "string") $0))))

; === primitive-application / primitive-term

(define-rule-syntax (primitive-test id param ...)
  (primitive-term id param ...))

(check
  (test=?
    (term-apply
      (primitive-test string-append x y)
      "foo" "bar")
    "foobar"))

(check
  (test=?
    (term-apply
      (primitive-test string-append x y)
      (variable 0) (variable 1))
    (primitive-application 'string-append
      (list (variable 0) (variable 1)))))

; === type-constructor / type-term

(check
  (test=?
    (term-apply
      (type-term pair car cdr)
      (variable 0) (variable 1))
    (type-constructor 'pair
      (list (variable 0) (variable 1)))))

; === tuple-constructor / tuple-term

(check
  (test=?
    (term-apply
      (tuple-term x y)
      (variable 0) (variable 1))
    (tuple-constructor
      (list (variable 0) (variable 1)))))

; === tuple-projection / tuple-ref-term

(check
  (test=?
    (term-apply
      (tuple-ref-term 1)
      (term-apply (tuple-term a b c d) "a" "b" "c" "d"))
    "b"))

(check
  (test=?
    (term-apply
      (tuple-ref-term 1)
      (variable 0))
    (tuple-projection (variable 0) 1)))

(check
  (test=?
    (term-apply
      (tuple-ref-term 1)
      (term-apply (tuple-term a b c d) "a" (variable 1) "c" "d"))
    (tuple-projection
      (tuple-constructor (list "a" (variable 1) "c" "d"))
      1)))

; === union-constructor / union-term

(check
  (test=?
    (term-apply (union-term 1 x) "foo")
    (union-constructor 1 "foo")))

(check
  (test=?
    (term-apply (union-term 1 x) (variable 0))
    (union-constructor 1 (variable 0))))

; === union-eliminator / union-case-term

(check
  (test=?
    (term-apply
      (union-case-term x f0 f1 f2 f3)
      (term-apply (union-term 1 x) "one")
      (variable 0)
      (abstraction* x x)
      (variable 2)
      (variable 3))
    "one"))

(check
  (test=?
    (term-apply
      (union-case-term x f0 f1 f2 f3)
      (term-apply (union-term 1 x) "one")
      (variable 0)
      (abstraction* x 20)
      (variable 2)
      (variable 3))
    20))

(check
  (test=?
    (term-apply
      (union-case-term x f0 f1 f2 f3)
      (variable 0)
      (variable 1)
      (abstraction* x x)
      (variable 2)
      (variable 3))
    (union-eliminator
      (variable 0)
      (list
        (variable 1)
        (abstraction* x x)
        (variable 2)
        (variable 3)))))

(check
  (test=?
    (term-apply
      (union-case-term x f0 f1 f2 f3)
      (term-apply (union-term 1 x) "one")
      (abstraction* x x)
      (variable 1)
      (abstraction* x x)
      (abstraction* x x))
    (union-eliminator
      (union-constructor 1 "one")
      (list
        (abstraction* x x)
        (variable 1)
        (abstraction* x x)
        (abstraction* x x)))))
