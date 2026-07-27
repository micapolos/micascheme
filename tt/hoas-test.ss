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
    (test->datum (native "foo"))
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
    (syntax->datum (test->syntax (native "foo")))
    '(native "foo")))

(check
  (equal?
    (syntax->datum
      (test->syntax
        (abstraction
          (lambda ($v0)
            (abstraction
              (lambda ($v1)
                (arrow $v0 $v1)))))))
    '(abstraction (lambda ($0)
      (abstraction (lambda ($1)
        (arrow $0 $1)))))))

; === term=?

(define (obj=? $depth $lhs $rhs)
  (equal? $lhs $rhs))

(define test=? (partial term=? obj=? 0))

(check
  (test=?
    (native "foo")
    (native "foo")))

(check
  (not
    (test=?
      (native "foo")
      (native "bar"))))

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
            (native (application $0 $1))))))
    (abstraction
      (lambda ($0)
        (abstraction
          (lambda ($1)
            (native (application $0 $1))))))))

; --- unify

(define (native-unify $subst $lhs $rhs)
  (and (equal? $lhs $rhs) $subst))

(define test-unify (partial unify native-unify))

(check
  (equal?
    (test-unify
      (list)
      (native 10)
      (native 10))
    (list)))

(check
  (equal?
    (test-unify
      (list)
      (native 10)
      (native 20))
    #f))

(check
  (equal?
    (test-unify
      (list #f)
      (hole 0)
      (native 10))
    (list (native 10))))

(check
  (equal?
    (test-unify
      (list #f)
      (native 10)
      (hole 0))
    (list (native 10))))

(check
  (equal?
    (test-unify
      (list (native 10))
      (native 10)
      (hole 0))
    (list (native 10))))

(check
  (equal?
    (test-unify
      (list (native 20))
      (native 10)
      (hole 0))
    #f))

(check
  (equal?
    (test-unify
      (list)
      (abstraction (lambda ($arg) $arg))
      (native 10))
    (list (native 10))))

(check
  (equal?
    (test-unify
      (list)
      (application (native 10) (native 20))
      (application (native 10) (native 20)))
    (list)))

(check
  (equal?
    (test-unify
      (list #f #f)
      (application (hole 0) (hole 1))
      (application (native 10) (native 20)))
    (list (native 20) (native 10))))

(check
  (equal?
    (test-unify
      (list #f)
      (application (hole 0) (hole 0))
      (application (native 10) (native 10)))
    (list (native 10))))

(check
  (equal?
    (test-unify
      (list #f #f)
      (application (hole 0) (hole 0))
      (application (native 10) (native 20)))
    #f))

; --- instantiate

(lets
  ((values $subst $term)
    (instantiate
      (list (native "foo"))
      (abstraction
        (lambda ($0)
          (abstraction
            (lambda ($1)
              (application $0 $1)))))))
  (run
    (check (equal? $subst (list #f #f (native "foo"))))
    (check (equal? $term (application (hole 1) (hole 2))))))

; --- subst-apply

(define (native-apply $subst $obj)
  (native $obj))

(define test-subst-apply (partial subst-apply native-apply))

(check
  (equal?
    (test-subst-apply
      (list (native "foo"))
      (application (native 10) (hole 0)))
    (application (native 10) (native "foo"))))

(check
  (equal?
    (test-subst-apply
      (list (native "foo") (hole 1))
      (application (native 10) (hole 0)))
    (application (native 10) (native "foo"))))

(check
  (equal?
    (test-subst-apply
      (list (native "foo") #f)
      (application (native 10) (hole 0)))
    (application (native 10) (hole 0))))

; --- term-replace

(define (obj-replace $obj $replaced-hole $replacement-term)
  (native $obj))

(define test-replace (partial term-replace obj-replace))

(check
  (equal?
    (test-replace
      (hole 1)
      (hole 1)
      (native "20"))
    (native "20")))

(check
  (equal?
    (test-replace
      (hole 1)
      (hole 2)
      (native "20"))
    (hole 1)))

(check
  (equal?
    (test->datum
      (test-replace
        (abstraction (lambda ($arg) (hole 1)))
        (hole 1)
        (native "20")))
    (test->datum
      (abstraction (lambda ($arg) (native "20"))))))

(check
  (equal?
    (test->datum
      (test-replace
        (abstraction
          (lambda ($arg)
            (application (hole 0) (hole 1))))
        (hole 1)
        (native "20")))
    (test->datum
      (abstraction
        (lambda ($arg)
          (application (hole 0) (native "20")))))))

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
        (hole 10)
        (hole 11)))
    '(lambda v0 v10)))

(check
  (equal?
    (test->datum
      (test-generalize
        (application (hole 10) (hole 1))
        (hole 1)))
    '(lambda v0 (v10 v0))))

; --- test +

(define (make-inc-term)
  (abstraction
    (lambda ($arg)
      (switch $arg
        ((native? $native)
          (native (+ (native-ref $native) 1)))
        ((else $other)
          (application (native +) $other))))))

(check
  (equal?
    (term-apply (make-inc-term) (native 10))
    (native 11)))

(check
  (equal?
    (term-apply (make-inc-term) (hole 10))
    (application (native +) (hole 10))))

(check
  (equal?
    (term-apply (hole 10) (hole 20))
    (application (hole 10) (hole 20))))

(check (not (equal? (make-inc-term) (make-inc-term))))
(check (test=? (make-inc-term) (make-inc-term)))
