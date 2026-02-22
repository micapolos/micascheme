(import
  (leo2 base)
  (leo2 term)
  (leo2 equal))

; nothing
(check (term=? nothing anything))
(check (not (term=? nothing nothing)))
(check (not (term=? nothing (variable 'x))))

; anything
(check (term=? anything nothing))
(check (term=? anything (variable 'x)))

; type
(check (term=? (type 1) (type 1)))
(check (not (term=? (type 1) (type 2))))
(check (not (term=? (type 1) (variable 'x))))

; symbol
(check (term=? 'foo 'foo))
(check (not (term=? 'foo 'bar)))
(check (not (term=? 'foo (native 'foo))))

; symbolic
(check
  (term=?
    (symbolic 'foo (variable 'x))
    (symbolic 'foo (variable 'x))))

(check
  (not
    (term=?
      (symbolic 'foo (variable 'x))
      (symbolic 'foo2 (variable 'x)))))

(check
  (not
    (term=?
      (symbolic 'foo (variable 'x))
      (symbolic 'foo (variable 'y)))))

; indexed
(check
  (term=?
    (indexed 2 (variable 'x))
    (indexed 2 (variable 'x))))

(check
  (not
    (term=?
      (indexed 2 (variable 'x))
      (indexed 3 (variable 'x)))))

(check
  (not
    (term=?
      (indexed 2 (variable 'x))
      (indexed 2 (variable 'y)))))

; native
(check (term=? (native 10) (native 10)))
(check (not (term=? (native 10) (native 12))))
(check (not (term=? (native 10) (variable 'x))))

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
(check (term=? (variable 'x) (variable 'x)))
(check (not (term=? (variable 'x) (variable 'y))))
(check (not (term=? (variable 'x) 'x)))

; abstraction
(check
  (term=?
    (abstraction (lambda (x) x))
    (abstraction (lambda (x) x))))

(check
  (term=?
    (abstraction (lambda (x) (abstraction (lambda (y) x))))
    (abstraction (lambda (x) (abstraction (lambda (y) x))))))

(check
  (not
    (term=?
      (abstraction (lambda (x) x))
      (abstraction (lambda (x) (variable 'x))))))

(check
  (not
    (term=?
      (abstraction (lambda (x) x))
      (abstraction (lambda (x) (abstraction (lambda (y) x)))))))

(check
  (not
    (term=?
      (abstraction (lambda (x) (abstraction (lambda (y) x))))
      (abstraction (lambda (x) (abstraction (lambda (y) y)))))))

; signature
(check
  (term=?
    (signature (type 0) (lambda (x) x))
    (signature (type 0) (lambda (x) x))))

(check
  (term=?
    (signature (type 0) (lambda (x) (signature (type 1) (lambda (y) x))))
    (signature (type 0) (lambda (x) (signature (type 1) (lambda (y) x))))))

(check
  (term=?
    (signature (type 0) (lambda (x) (signature x (lambda (y) x))))
    (signature (type 0) (lambda (x) (signature x (lambda (y) x))))))

(check
  (not
    (term=?
      (signature (type 0) (lambda (x) x))
      (signature (type 1) (lambda (x) x)))))

(check
  (not
    (term=?
      (signature (type 0) (lambda (x) x))
      (signature (type 0) (lambda (x) (variable 'x))))))

(check
  (not
    (term=?
      (signature (type 0) (lambda (x) x))
      (signature (type 0) (lambda (x) (signature x (lambda (y) x)))))))

(check
  (not
    (term=?
      (signature (type 0) (lambda (x) (signature (type 0) (lambda (y) x))))
      (signature (type 0) (lambda (x) (signature (type 0) (lambda (y) y)))))))

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
    (branch (variable 'x) (variable 'y) (variable 'z))
    (branch (variable 'x) (variable 'y) (variable 'z))))

(check
  (not
    (term=?
      (branch (variable 'x) (variable 'y) (variable 'z))
      (branch (variable 'x2) (variable 'y) (variable 'z)))))

(check
  (not
    (term=?
      (branch (variable 'x) (variable 'y) (variable 'z))
      (branch (variable 'x2) (variable 'y2) (variable 'z)))))

(check
  (not
    (term=?
      (branch (variable 'x) (variable 'y) (variable 'z))
      (branch (variable 'x) (variable 'y) (variable 'z2)))))

; recursion
(check
  (term=?
    (recursion (lambda (x) x))
    (recursion (lambda (x) x))))

(check
  (term=?
    (recursion (lambda (x) (abstraction (lambda (y) x))))
    (recursion (lambda (x) (abstraction (lambda (y) x))))))

(check
  (not
    (term=?
      (recursion (lambda (x) x))
      (recursion (lambda (x) (variable 'x))))))

(check
  (not
    (term=?
      (recursion (lambda (x) x))
      (recursion (lambda (x) (abstraction (lambda (y) x)))))))

(check
  (not
    (term=?
      (recursion (lambda (x) (abstraction (lambda (y) x))))
      (recursion (lambda (x) (abstraction (lambda (y) y)))))))

; TODO: annotated, evaluated, typed
