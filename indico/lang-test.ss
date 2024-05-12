(import (scheme) (check) (indico lang) (list) (lets) (list-syntax))

; === native

(check
  (equal?
    (values->list (indico (native)))
    (list)))

(check
  (equal?
    (indico (native "foo"))
    "foo"))

(check
  (equal?
    (values->list (indico (native "foo" "bar")))
    (list "foo" "bar")))

; === block / variable

(check
  (equal?
    (values->list (indico (block ())))
    (list)))

(check
  (equal?
    (indico (block ((native "foo")) (variable 0)))
    "foo"))

(check
  (equal?
    (indico (block ((native "foo") (native "bar")) (variable 1)))
    "foo"))

(check
  (equal?
    (indico (block ((native "foo") (native "bar")) (variable 0)))
    "bar"))

(check
  (equal?
    (values->list (indico (block ((native "foo") (native "bar")) (variable 1) (variable 0))))
    (list "foo" "bar")))

(check
  (equal?
    (values->list (indico (block ((native "foo" "bar") (native "goo")) (variable 2) (variable 1) (variable 0))))
    (list "foo" "bar" "goo")))

; === function

(check
  (equal?
    ((indico (function 0 (native "foo"))))
    "foo"))

(check
  (equal?
    ((indico (function 0 (native "foo"))))
    "foo"))

(check
  (equal?
    ((indico (function 1 (variable 0))) "foo")
    "foo"))

(check
  (equal?
    ((indico (function 2 (variable 1))) "foo" "bar")
    "foo"))

(check
  (equal?
    ((indico (function 2 (variable 0))) "foo" "bar")
    "bar"))

; === call

(check
  (equal?
    (values->list (indico (call (function 0))))
    (list)))

(check
  (equal?
    (indico (call (function 0 (native "foo"))))
    "foo"))

(check
  (equal?
    (values->list (indico (call (function 0 (native "foo") (native "bar")))))
    (list "foo" "bar")))

(check
  (equal?
    (values->list (indico (call (function 0 (native "foo" "bar")))))
    (list "foo" "bar")))

(check
  (equal?
    (indico (call (function 1 (variable 0)) (native "foo")))
    "foo"))

(check
  (equal?
    (indico (call (function 2 (variable 1)) (native "foo") (native "bar")))
    "foo"))

(check
  (equal?
    (indico (call (function 2 (variable 0)) (native "foo") (native "bar")))
    "bar"))

(check
  (equal?
    (indico (call (function 2 (variable 1)) (native "foo" "bar")))
    "foo"))

(check
  (equal?
    (indico (call (function 2 (variable 0)) (native "foo" "bar")))
    "bar"))

; === recursive function

(lets
  ($procedure (indico (recursive (function 0 (variable 0)))))
  (check (equal? $procedure ($procedure))))

