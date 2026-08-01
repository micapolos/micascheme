(import
  (scheme)
  (check)
  (procedure)
  (switch)
  (data)
  (lets)
  (list)
  (boolean)
  (tt hoas))

; === term->datum

(define (obj->datum $depth $obj) $obj)

(define test->datum (partial term->datum obj->datum 0))

(check
  (equal?
    (test->datum "foo")
    '"foo"))

(check
  (equal?
    (test->datum (hole 0))
    'v0))

(check
  (equal?
    (test->datum
      (abstraction (lambda ($arg) $arg)))
    '(lambda v0 v0)))

(check
  (equal?
    (test->datum
      (abstraction
        (lambda ($v0)
          (abstraction
            (lambda ($v1)
              (application $v0 $v1))))))
    '(lambda v0 (lambda v1 (v0 v1)))))

; === term->syntax

(define (obj->syntax $depth $obj) (datum->syntax #'+ $obj))

(define test->syntax (partial term->syntax obj->syntax 0))

(check
  (equal?
    (syntax->datum (test->syntax "foo"))
    '"foo"))

(check
  (equal?
    (syntax->datum
      (test->syntax
        (abstraction
          (lambda ($v0)
            (abstraction
              (lambda ($v1)
                (application $v0 $v1)))))))
    '(abstraction (lambda ($0)
      (abstraction (lambda ($1)
        (application $0 $1)))))))

; === term=?

(define (obj=? $depth $lhs $rhs)
  (equal? $lhs $rhs))

(define test=? (partial term=? obj=? 0))

(check (test=? "foo" "foo"))

(check (not (test=? "foo" "bar")))

(check
  (test=?
    (hole 0)
    (hole 0)))

(check
  (not
    (test=?
      (hole 0)
      (hole 1))))

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

; --- unify

(define (native-unify $subst $lhs $rhs)
  (and (equal? $lhs $rhs) $subst))

(define test-unify (partial unify native-unify))

(check
  (equal?
    (test-unify (list) 10 10)
    (list)))

(check
  (equal?
    (test-unify (list) 10 20)
    #f))

(check
  (equal?
    (test-unify (list #f) (hole 0) 10)
    (list 10)))

(check
  (equal?
    (test-unify (list #f) 10 (hole 0))
    (list 10)))

(check
  (equal?
    (test-unify (list 10) 10 (hole 0))
    (list 10)))

(check
  (equal?
    (test-unify (list 20) 10 (hole 0))
    #f))

(check
  (equal?
    (test-unify
      (list)
      (abstraction (lambda ($arg) $arg))
      10)
    (list 10)))

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
      (list #f #f)
      (application (hole 0) (hole 1))
      (application 10 20))
    (list 20 10)))

(check
  (equal?
    (test-unify
      (list #f)
      (application (hole 0) (hole 0))
      (application 10 10))
    (list 10)))

(check
  (equal?
    (test-unify
      (list #f #f)
      (application (hole 0) (hole 0))
      (application 10 20))
    #f))

; --- instantiate

(lets
  ((values $subst $term)
    (instantiate
      (list "foo")
      (abstraction
        (lambda ($0)
          (abstraction
            (lambda ($1)
              (application $0 $1)))))))
  (run
    (check (equal? $subst (list #f #f "foo")))
    (check (equal? $term (application (hole 1) (hole 2))))))

; --- subst-apply

(define (native-apply $subst $obj) $obj)

(define test-subst-apply (partial subst-apply native-apply))

(check
  (equal?
    (test-subst-apply
      (list "foo")
      (application 10 (hole 0)))
    (application 10 "foo")))

(check
  (equal?
    (test-subst-apply
      (list "foo" (hole 1))
      (application 10 (hole 0)))
    (application 10 "foo")))

(check
  (equal?
    (test-subst-apply
      (list "foo" #f)
      (application 10 (hole 0)))
    (application 10 (hole 0))))

; --- term-replace

(define (obj-replace $replaced-hole $replacement-term $obj) $obj)

(define test-replace (partial term-replace obj-replace))

(check
  (equal?
    (test-replace
      (hole 1)
      "20"
      (hole 1))
    "20"))

(check
  (equal?
    (test-replace
      (hole 2)
      "20"
      (hole 1))
    (hole 1)))

(check
  (equal?
    (test->datum
      (test-replace
        (hole 1)
        "20"
        (abstraction (lambda ($arg) (hole 1)))))
    (test->datum
      (abstraction (lambda ($arg) "20")))))

(check
  (equal?
    (test->datum
      (test-replace
        (hole 1)
        "20"
        (abstraction
          (lambda ($arg)
            (application (hole 0) (hole 1))))))
    (test->datum
      (abstraction
        (lambda ($arg)
          (application (hole 0) "20"))))))

; --- append-term-holes

(define (append-obj-holes $depth $holes $obj)
  $holes)

(define append-test-holes (partial append-term-holes append-obj-holes))

(check
  (equal?
    (append-test-holes 10
      (list (hole 20))
      (hole 9))
    (list (hole 9) (hole 20))))

(check
  (equal?
    (append-test-holes 10
      (list (hole 20))
      (hole 10))
    (list (hole 20))))

(check
  (equal?
    (append-test-holes 10
      (list (hole 20))
      (application (hole 8) (hole 9)))
    (list (hole 9) (hole 8) (hole 20))))

(check
  (equal?
    (append-test-holes 10
      (list (hole 20))
      (application (hole 9) (hole 9)))
    (list (hole 9) (hole 20))))

(check
  (equal?
    (append-test-holes 10
      (list (hole 20))
      (abstraction (lambda ($arg)
        (application $arg (hole 9)))))
    (list (hole 9) (hole 20))))

; --- term-generalize

(define test-generalize (partial term-generalize obj-replace))

(check
  (equal?
    (test->datum
      (test-generalize
        (hole 10)
        (hole 10)))
    '(lambda v0 v0)))

(check
  (equal?
    (test->datum
      (test-generalize
        (hole 11)
        (hole 10)))
    '(lambda v0 v10)))

(check
  (equal?
    (test->datum
      (test-generalize
        (hole 1)
        (application (hole 10) (hole 1))))
    '(lambda v0 (v10 v0))))

; --- test +

(define (make-inc-term)
  (abstraction
    (lambda ($arg)
      (switch $arg
        ((number? $number)
          (+ $number 1))
        ((else $other)
          (application make-inc-term $other))))))

(check
  (equal?
    (term-apply (make-inc-term) 10)
    11))

(check
  (equal?
    (term-apply (make-inc-term) (hole 10))
    (application make-inc-term (hole 10))))

(check
  (equal?
    (term-apply (hole 10) (hole 20))
    (application (hole 10) (hole 20))))

(check (test=? (make-inc-term) (make-inc-term)))
