(import (leo2 base) (leo2 term) (leo2 solver))

(define depth-0 0)

(check-solver=?
  (solver (native "foo"))
  (solver (native "foo")))

(check-solver=?
  (solver-lets (solver (native "foo")))
  (solver (native "foo")))

(check-solver=?
  (solver-lets
    ($foo (solver "foo"))
    ($bar (solver "bar"))
    (solver (string-append $foo $bar)))
  (solver "foobar"))

(check-solver=?
  (list->solver
    (list
      (solver (native "foo"))
      (solver (native "bar"))))
  (solver
    (list
      (native "foo")
      (native "bar"))))

(check-solver=?
  (apply-solver cons
    (solver (native "foo"))
    (solver (native "bar")))
  (solver
    (cons
      (native "foo")
      (native "bar"))))

; ====== term-solver =======

; rhs hole
(check-solver=?
  (set-solutions-solver
    (stack (native "foo"))
    (term-solver depth-0 (native "foo") (hole 0)))
  (solver-with
    (stack (native "foo"))
    (native "foo")))

; hole
(check-solver=?
  (term-solver depth-0 (hole 0) (native "foo"))
  (solver (unbound (hole 0))))

(check-solver=?
  (set-solutions-solver
    (stack native-type)
    (term-solver depth-0 (hole 0) native-type))
  (solver-with
    (stack native-type)
    native-type))

(check-solver=?
  (set-solutions-solver
    (stack (native "foo"))
    (term-solver depth-0 (hole 0) (native "bar")))
  (solver-with
    (stack (native "foo"))
    (mismatch (native "foo") (native "bar"))))

; unknown
(check-solver=?
  (term-solver depth-0 unknown unknown)
  (solver unknown))

(check-solver=?
  (term-solver depth-0 unknown native-type)
  (solver native-type))

; native-type
(check-solver=?
  (term-solver depth-0 native-type native-type)
  (solver native-type))

(check-solver=?
  (term-solver depth-0 native-type (variable 0))
  (solver (mismatch native-type (variable 0))))

; type
(check-solver=?
  (term-solver depth-0 (type 0) (type 0))
  (solver (type 0)))

(check-solver=?
  (term-solver depth-0 (type 0) (type 1))
  (solver (mismatch (type 0) (type 1))))

(check-solver=?
  (term-solver depth-0 (type 0) (variable 0))
  (solver (mismatch (type 0) (variable 0))))

; native
(check-solver=?
  (term-solver depth-0 (native "foo") (native "foo"))
  (solver (native "foo")))

(check-solver=?
  (term-solver depth-0 (native "foo") (native "bar"))
  (solver (mismatch (native "foo") (native "bar"))))

(check-solver=?
  (term-solver depth-0 (native "foo") (variable 0))
  (solver (mismatch (native "foo") (variable 0))))

; evaluated
(check-solver=?
  (term-solver depth-0
    (evaluated (native "foo"))
    (evaluated (native "foo")))
  (solver (evaluated (native "foo"))))

(check-solver=?
  (term-solver depth-0
    (evaluated (native "foo"))
    (evaluated (native "bar")))
  (solver
    (evaluated
      (mismatch
        (native "foo")
        (native "bar")))))

; typed
(check-solver=?
  (term-solver depth-0
    (typed (native "t1") (native "v1"))
    (typed (native "t1") (native "v1")))
  (solver
    (typed (native "t1") (native "v1"))))

(check-solver=?
  (term-solver depth-0
    (typed (native "t1") (native "v1"))
    (typed (native "t1") (native "v2")))
  (solver
    (typed
      (native "t1")
      (mismatch (native "v1") (native "v2")))))

(check-solver=?
  (term-solver depth-0
    (typed (native "t1") (native "v1"))
    (typed (native "t2") (native "v1")))
  (solver
    (typed
      (mismatch (native "t1") (native "t2"))
      (native "v1"))))

(check-solver=?
  (term-solver depth-0
    (typed (native "t1") (native "v1"))
    (typed (native "t2") (native "v2")))
  (solver
    (typed
      (mismatch (native "t1") (native "t2"))
      (mismatch (native "v1") (native "v2")))))

(check-solver=?
  (term-solver depth-0
    (typed (native "t1") (native "v1"))
    (native "v1"))
  (solver
    (mismatch
      (typed (native "t1") (native "v1"))
      (native "v1"))))

; lambda
(check-solver=?
  (term-solver depth-0
    (lambda ($0) $0)
    (lambda ($0) $0))
  (solver (lambda ($0) $0)))

(check-solver=?
  (term-solver depth-0
    (lambda ($0) (lambda ($1) $0))
    (lambda ($0) (lambda ($1) $0)))
  (solver (lambda ($0) (lambda ($1) $0))))

(check-solver=?
  (term-solver depth-0
    (lambda ($0) (lambda ($1) $1))
    (lambda ($0) (lambda ($1) $1)))
  (solver (lambda ($0) (lambda ($1) $1))))

(check-solver=?
  (term-solver depth-0
    (lambda ($0) (lambda ($1) $1))
    (lambda ($0) (lambda ($1) $0)))
  (solver
    (lambda ($0) (lambda ($1) (mismatch $1 $0)))))

(check-solver=?
  (term-solver depth-0
    (lambda ($0) $0)
    (variable 0))
  (solver
    (mismatch
      (lambda ($0) $0)
      (variable 0))))

; lambda-type
(check-solver=?
  (term-solver depth-0
    (lambda-type native-type (lambda ($0) $0))
    (lambda-type native-type (lambda ($0) $0)))
  (solver
    (lambda-type native-type
      (lambda ($0) $0))))

(check-solver=?
  (term-solver depth-0
    (lambda-type native-type (lambda ($0) $0))
    (lambda-type native-type (lambda ($0) native-type)))
  (solver
    (lambda-type native-type
      (lambda ($0)
        (mismatch $0 native-type)))))

; application
(check-solver=?
  (term-solver depth-0
    (application (native "foo") (native "bar"))
    (application (native "foo") (native "bar")))
  (solver
    (application (native "foo") (native "bar"))))

(check-solver=?
  (term-solver depth-0
    (application (native "foo") (native "foo"))
    (application (native "foo") (native "bar")))
  (solver
    (application
      (native "foo")
      (mismatch (native "foo") (native "bar")))))

(check-solver=?
  (term-solver depth-0
    (application (native "foo") (native "bar"))
    (application (native "bar") (native "bar")))
  (solver
    (application
      (mismatch (native "foo") (native "bar"))
      (native "bar"))))

(check-solver=?
  (set-solutions-solver
    (stack unknown)
    (term-solver depth-0
      (application (hole 0) (hole 0))
      (application (native "foo") (native "foo"))))
  (solver-with
    (stack (native "foo"))
    (application (native "foo") (native "foo"))))

(check-solver=?
  (set-solutions-solver
    (stack unknown)
    (term-solver depth-0
      (application (hole 0) (hole 0))
      (application (native "foo") (native "bar"))))
  (solver-with
    (stack (native "foo"))
    (application
      (native "foo")
      (mismatch (native "foo") (native "bar")))))

(check-solver=?
  (set-solutions-solver
    (stack unknown unknown)
    (term-solver depth-0
      (application (hole 0) (hole 1))
      (application (native "foo") (native "bar"))))
  (solver-with
    (stack (native "foo") (native "bar"))
    (application (native "foo") (native "bar"))))

(check-solver=?
  (set-solutions-solver
    (stack (native "foo"))
    (term-solver depth-0
      (application (hole 0) (native "bar"))
      (application (native "foo") (native "bar"))))
  (solver-with
    (stack (native "foo"))
    (application (native "foo") (native "bar"))))

(check-solver=?
  (set-solutions-solver
    (stack (native "foo"))
    (term-solver depth-0
      (application (hole 0) (hole 0))
      (application (native "foo") (native "foo"))))
  (solver-with
    (stack (native "foo"))
    (application (native "foo") (native "foo"))))

(check-solver=?
  (set-solutions-solver
    (stack (native "foo"))
    (term-solver depth-0
      (application (hole 0) (hole 0))
      (application (native "foo") (native "bar"))))
  (solver-with
    (stack (native "foo"))
    (application
      (native "foo")
      (mismatch (native "foo") (native "bar")))))

; recursion
(check-solver=?
  (term-solver depth-0
    (recursion (lambda ($0) (lambda ($1) $0)))
    (recursion (lambda ($0) (lambda ($1) $0))))
  (solver
    (recursion (lambda ($0) (lambda ($1) $0)))))

(check-solver=?
  (term-solver depth-0
    (recursion (lambda ($0) (lambda ($1) $1)))
    (recursion (lambda ($0) (lambda ($1) $1))))
  (solver
    (recursion (lambda ($0) (lambda ($1) $1)))))

(check-solver=?
  (term-solver depth-0
    (recursion (lambda ($0) (lambda ($1) $1)))
    (recursion (lambda ($0) (lambda ($1) $0))))
  (solver
    (recursion
      (lambda ($0) (lambda ($1) (mismatch $1 $0))))))
