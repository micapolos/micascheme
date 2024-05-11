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

; === block / get
(check
  (equal?
    (indico (block ()))
    (list)))

(check
  (equal?
    (indico (block ((native "foo")) (get 0)))
    (list "foo")))

(check
  (equal?
    (indico (block ((native "foo") (native "bar")) (get 1)))
    (list "foo")))

(check
  (equal?
    (indico (block ((native "foo") (native "bar")) (get 0)))
    (list "bar")))

(check
  (equal?
    (indico (block ((native "foo") (native "bar")) (get 1) (get 0)))
    (list "foo" "bar")))

(check
  (equal?
    (indico (block ((native "foo" "bar") (native "goo")) (get 2) (get 1) (get 0)))
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
      ($function (indico (function 1 (get 0))))
      ($function "foo"))
    (list "foo")))

(check
  (equal?
    (map-with
      ($function (indico (function 2 (get 1))))
      ($function "foo" "bar"))
    (list "foo")))

(check
  (equal?
    (map-with
      ($function (indico (function 2 (get 0))))
      ($function "foo" "bar"))
    (list "bar")))

; === apply
