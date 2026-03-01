(import
  (leo2 base)
  (leo2 term)
  (leo2 equal))

; hole
(check (term=? (hole 12) (hole 12)))
(check (not (term=? (hole 12) (hole 13))))
(check (not (term=? (hole 12) (variable 13))))

; nothing
(check (not (term=? nothing nothing)))
(check (not (term=? nothing (variable 0))))

; type
(check (term=? (type 1) (type 1)))
(check (not (term=? (type 1) (type 2))))
(check (not (term=? (type 1) (variable 0))))

; native
(check (term=? (native 10) (native 10)))
(check (not (term=? (native 10) (native 12))))
(check (not (term=? (native 10) (variable 0))))

; native-application
(check
  (term=?
    (native-application string-append (list (native "foo") (native "bar")))
    (native-application string-append (list (native "foo") (native "bar")))))

(check
  (not
    (term=?
      (native-application + (list (native "foo") (native "bar")))
      (native-application string-append (list (native "foo") (native "bar"))))))

(check
  (not
    (term=?
      (native-application string-append (list (native "foo")))
      (native-application string-append (list (native "foo") (native "bar"))))))

(check
  (not
    (term=?
      (native-application string-append (list (native "foo") (native "bar")))
      (native-application string-append (list (native "foo2") (native "bar"))))))

(check
  (not
    (term=?
      (native-application string-append (list (native "foo") (native "bar")))
      (native-application string-append (list (native "foo") (native "bar2"))))))

; variable
(check (term=? (variable 0) (variable 0)))
(check (not (term=? (variable 0) (variable 1))))
(check (not (term=? (variable 0) 0)))

; lambda
(check
  (term=?
    (lambda (x) x)
    (lambda (x) x)))

(check
  (term=?
    (lambda (x) (lambda (y) x))
    (lambda (x) (lambda (y) x))))

(check
  (not
    (term=?
      (lambda (x) x)
      (lambda (x) (variable 1)))))

(check
  (not
    (term=?
      (lambda (x) x)
      (lambda (x) (lambda (y) x)))))

(check
  (not
    (term=?
      (lambda (x) (lambda (y) x))
      (lambda (x) (lambda (y) y)))))

; lambda-type
(check
  (term=?
    (lambda-type (type 0) (lambda (x) x))
    (lambda-type (type 0) (lambda (x) x))))

(check
  (term=?
    (lambda-type (type 0) (lambda (x) (lambda-type (type 1) (lambda (y) x))))
    (lambda-type (type 0) (lambda (x) (lambda-type (type 1) (lambda (y) x))))))

(check
  (term=?
    (lambda-type (type 0) (lambda (x) (lambda-type x (lambda (y) x))))
    (lambda-type (type 0) (lambda (x) (lambda-type x (lambda (y) x))))))

(check
  (not
    (term=?
      (lambda-type (type 0) (lambda (x) x))
      (lambda-type (type 1) (lambda (x) x)))))

(check
  (not
    (term=?
      (lambda-type (type 0) (lambda (x) x))
      (lambda-type (type 0) (lambda (x) (variable 1))))))

(check
  (not
    (term=?
      (lambda-type (type 0) (lambda (x) x))
      (lambda-type (type 0) (lambda (x) (lambda-type x (lambda (y) x)))))))

(check
  (not
    (term=?
      (lambda-type (type 0) (lambda (x) (lambda-type (type 0) (lambda (y) x))))
      (lambda-type (type 0) (lambda (x) (lambda-type (type 0) (lambda (y) y)))))))

; application
(check
  (term=?
    (application (native 1) (native 2))
    (application (native 1) (native 2))))

(check
  (not
    (term=?
      (application (native 1) (native 2))
      (application (native 2) (native 2)))))

(check
  (not
    (term=?
      (application (native 1) (native 2))
      (application (native 1) (native 1)))))

; branch
(check
  (term=?
    (branch (variable 0) (variable 1) (variable 2))
    (branch (variable 0) (variable 1) (variable 2))))

(check
  (not
    (term=?
      (branch (variable 0) (variable 1) (variable 2))
      (branch (variable 02) (variable 1) (variable 2)))))

(check
  (not
    (term=?
      (branch (variable 0) (variable 1) (variable 2))
      (branch (variable 02) (variable 12) (variable 2)))))

(check
  (not
    (term=?
      (branch (variable 0) (variable 1) (variable 2))
      (branch (variable 0) (variable 1) (variable 22)))))

; recursion
(check
  (term=?
    (recursion (lambda (x) x))
    (recursion (lambda (x) x))))

(check
  (term=?
    (recursion (lambda (x) (lambda (y) x)))
    (recursion (lambda (x) (lambda (y) x)))))

(check
  (not
    (term=?
      (recursion (lambda (x) x))
      (recursion (lambda (x) (variable 1))))))

(check
  (not
    (term=?
      (recursion (lambda (x) x))
      (recursion (lambda (x) (lambda (y) x))))))

(check
  (not
    (term=?
      (recursion (lambda (x) (lambda (y) x)))
      (recursion (lambda (x) (lambda (y) y))))))

; TODO: labeled, evaluated, typed
