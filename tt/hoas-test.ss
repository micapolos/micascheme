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

(data (application lhs rhs))

; === term->datum

(define (obj->datum $depth $obj)
  (switch $obj
    ((application? $application)
      `(
        ,(term->datum obj->datum $depth (application-lhs $application))
        ,(term->datum obj->datum $depth (application-rhs $application))))
    ((else $other)
      $other)))

(define test->datum (partial term->datum obj->datum 0))

(check
  (equal?
    (test->datum (native "foo"))
    '"foo"))

(check
  (equal?
    (test->datum (variable 0))
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
              (native (application $v0 $v1)))))))
    '(lambda v0 (lambda v1 (v0 v1)))))

; === term=?

(define (obj=? $depth $lhs $rhs)
  (switch $lhs
    ((application? $lhs)
      (switch? $rhs
        ((application? $rhs)
          (and
            (term=? obj=? $depth
              (application-lhs $lhs)
              (application-lhs $rhs))
            (term=? obj=? $depth
              (application-rhs $lhs)
              (application-rhs $rhs))))))
    ((else $lhs)
      (equal? $lhs $rhs))))

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
    (variable 0)
    (variable 0)))

(check
  (not
    (test=?
      (variable 0)
      (variable 1))))

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
  (cond
    ((and (application? $lhs) (application? $rhs))
      (lets?
        ($subst (test-unify $subst (application-lhs $lhs) (application-lhs $rhs)))
        (test-unify $subst (application-rhs $lhs) (application-rhs $rhs))))
    (else
      (and (equal? $lhs $rhs) $subst))))

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
      (variable 0)
      (native 10))
    (list (native 10))))

(check
  (equal?
    (test-unify
      (list #f)
      (native 10)
      (variable 0))
    (list (native 10))))

(check
  (equal?
    (test-unify
      (list (native 10))
      (native 10)
      (variable 0))
    (list (native 10))))

(check
  (equal?
    (test-unify
      (list (native 20))
      (native 10)
      (variable 0))
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
      (native (application (native 10) (native 20)))
      (native (application (native 10) (native 20))))
    (list)))

(check
  (equal?
    (test-unify
      (list #f #f)
      (native (application (variable 0) (variable 1)))
      (native (application (native 10) (native 20))))
    (list (native 20) (native 10))))

(check
  (equal?
    (test-unify
      (list #f)
      (native (application (variable 0) (variable 0)))
      (native (application (native 10) (native 10))))
    (list (native 10))))

(check
  (equal?
    (test-unify
      (list #f #f)
      (native (application (variable 0) (variable 0)))
      (native (application (native 10) (native 20))))
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
              (native (application $0 $1))))))))
  (run
    (check (equal? $subst (list #f #f (native "foo"))))
    (check (equal? $term (native (application (variable 1) (variable 2)))))))

; --- subst-apply

(define (native-apply $subst $obj)
  (native
    (switch $obj
      ((application? $application)
        (application
          (test-subst-apply $subst (application-lhs $application))
          (test-subst-apply $subst (application-rhs $application))))
      ((else $other) $other))))

(define test-subst-apply (partial subst-apply native-apply))

(check
  (equal?
    (test-subst-apply
      (list (native "foo"))
      (native (application (native 10) (variable 0))))
    (native (application (native 10) (native "foo")))))

(check
  (equal?
    (test-subst-apply
      (list (native "foo") (variable 1))
      (native (application (native 10) (variable 0))))
    (native (application (native 10) (native "foo")))))

(check
  (equal?
    (test-subst-apply
      (list (native "foo") #f)
      (native (application (native 10) (variable 0))))
    (native (application (native 10) (variable 0)))))

; --- term-replace

(define (obj-replace $obj $replaced-variable $replacement-term)
  (switch $obj
    ((application? $application)
      (native
        (application
          (term-replace obj-replace (application-lhs $application) $replaced-variable $replacement-term)
          (term-replace obj-replace (application-rhs $application) $replaced-variable $replacement-term))))
    ((else $other) $other)))

(define test-replace (partial term-replace obj-replace))

(check
  (equal?
    (test-replace
      (variable 1)
      (variable 1)
      (native "20"))
    (native "20")))

(check
  (equal?
    (test-replace
      (variable 1)
      (variable 2)
      (native "20"))
    (variable 1)))

(check
  (equal?
    (test->datum
      (test-replace
        (abstraction (lambda ($arg) (variable 1)))
        (variable 1)
        (native "20")))
    (test->datum
      (abstraction (lambda ($arg) (native "20"))))))

(check
  (equal?
    (test->datum
      (test-replace
        (abstraction
          (lambda ($arg)
            (native (application (variable 0) (variable 1)))))
        (variable 1)
        (native "20")))
    (test->datum
      (abstraction
        (lambda ($arg)
          (native (application (variable 0) (native "20"))))))))

; --- append-term-variables

(define (append-obj-variables $depth $variables $obj)
  (switch $obj
    ((application? $application)
      (append-term-variables append-obj-variables $depth
        (append-term-variables append-obj-variables $depth $variables (application-lhs $application))
        (application-rhs $application)))
    ((else $other)
      $variables)))

(define append-test-variables (partial append-term-variables append-obj-variables))

(check
  (equal?
    (append-test-variables 10
      (list (variable 20))
      (variable 9))
    (list (variable 9) (variable 20))))

(check
  (equal?
    (append-test-variables 10
      (list (variable 20))
      (variable 10))
    (list (variable 20))))

(check
  (equal?
    (append-test-variables 10
      (list (variable 20))
      (native (application (variable 8) (variable 9))))
    (list (variable 9) (variable 8) (variable 20))))

(check
  (equal?
    (append-test-variables 10
      (list (variable 20))
      (native (application (variable 9) (variable 9))))
    (list (variable 9) (variable 9) (variable 20))))

(check
  (equal?
    (append-test-variables 10
      (list (variable 20))
      (abstraction (lambda ($arg)
        (native (application $arg (variable 9))))))
    (list (variable 9) (variable 20))))

; --- term-generalize

(define test-generalize (partial term-generalize obj-replace))

(check
  (equal?
    (test->datum
      (test-generalize
        (variable 10)
        (variable 10)))
    '(lambda v0 v0)))

(check
  (equal?
    (test->datum
      (test-generalize
        (variable 10)
        (variable 11)))
    '(lambda v0 v10)))

(check
  (equal?
    (test->datum
      (test-generalize
        (native (application (variable 10) (variable 1)))
        (variable 1)))
    '(lambda v0 (v10 v0))))
