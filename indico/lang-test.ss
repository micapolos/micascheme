(import (scheme) (check) (indico lang) (list-syntax))

; === native

(check
  (equal?
    (indico (native))
    (list)))

(check
  (equal?
    (indico (native "foo"))
    (list "foo")))

(check
  (equal?
    (indico (native "foo" "bar"))
    (list "foo" "bar")))

; === block / variable

(check
  (equal?
    (indico (block ()))
    (list)))

(check
  (equal?
    (indico (block ((native "foo")) (variable 0)))
    (list "foo")))

(check
  (equal?
    (indico (block ((native "foo") (native "bar")) (variable 1)))
    (list "foo")))

(check
  (equal?
    (indico (block ((native "foo") (native "bar")) (variable 0)))
    (list "bar")))

(check
  (equal?
    (indico (block ((native "foo") (native "bar")) (variable 1) (variable 0)))
    (list "foo" "bar")))

(check
  (equal?
    (indico (block ((native "foo" "bar") (native "goo")) (variable 2) (variable 1) (variable 0)))
    (list "foo" "bar" "goo")))

; === function

(check
  (equal?
    (map-with
      ($function (indico (function 0 (native "foo"))))
      ($function))
    (list "foo")))

(check
  (equal?
    (map-with
      ($function (indico (function 0 (native "foo"))))
      ($function))
    (list "foo")))

(check
  (equal?
    (map-with
      ($function (indico (function 1 (variable 0))))
      ($function "foo"))
    (list "foo")))

(check
  (equal?
    (map-with
      ($function (indico (function 2 (variable 1))))
      ($function "foo" "bar"))
    (list "foo")))

(check
  (equal?
    (map-with
      ($function (indico (function 2 (variable 0))))
      ($function "foo" "bar"))
    (list "bar")))

; === call

(check
  (equal?
    (indico (call (function 0)))
    (list)))

(check
  (equal?
    (indico (call (function 0 (native "foo"))))
    (list "foo")))

(check
  (equal?
    (indico (call (function 0 (native "foo") (native "bar"))))
    (list "foo" "bar")))

(check
  (equal?
    (indico (call (function 0 (native "foo" "bar"))))
    (list "foo" "bar")))

(check
  (equal?
    (indico (call (function 1 (variable 0)) (native "foo")))
    (list "foo")))

(check
  (equal?
    (indico (call (function 2 (variable 1)) (native "foo") (native "bar")))
    (list "foo")))

(check
  (equal?
    (indico (call (function 2 (variable 0)) (native "foo") (native "bar")))
    (list "bar")))

(check
  (equal?
    (indico (call (function 2 (variable 1)) (native "foo" "bar")))
    (list "foo")))

(check
  (equal?
    (indico (call (function 2 (variable 0)) (native "foo" "bar")))
    (list "bar")))
