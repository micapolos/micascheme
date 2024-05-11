(import (scheme) (check) (stack) (list) (syntax) (rust indexed))

(define-case-syntax (indexed body)
  #`(values->list
    #,(expr-syntax (syntax->expr (stack) #'body))))

; === native
(check
  (equal?
    (indexed (native))
    (list)))

(check
  (equal?
    (indexed (native "foo"))
    (list "foo")))

(check
  (equal?
    (indexed (native "foo" "bar"))
    (list "foo" "bar")))

; === block / get
(check
  (equal?
    (indexed (block ()))
    (list)))

(check
  (equal?
    (indexed (block ((native "foo")) (get 0)))
    (list "foo")))

(check
  (equal?
    (indexed (block ((native "foo") (native "bar")) (get 1)))
    (list "foo")))

(check
  (equal?
    (indexed (block ((native "foo") (native "bar")) (get 0)))
    (list "bar")))

(check
  (equal?
    (indexed (block ((native "foo") (native "bar")) (get 1) (get 0)))
    (list "foo" "bar")))

(check
  (equal?
    (indexed (block ((native "foo" "bar") (native "goo")) (get 2) (get 1) (get 0)))
    (list "foo" "bar" "goo")))
