(import (micascheme) (leo sentence))

(check
  (equal?
    (->sentence? '(foo))
    (sentence "foo" '())))

(check
  (equal?
    (->sentence? '(foo ()))
    (sentence "foo" '(()))))

(check
  (equal?
    (->sentence? '(foo bar))
    (sentence "foo" '(bar))))

(check
  (equal?
    (->sentence? '(foo (bar)))
    (sentence "foo" '((bar)))))

(check
  (equal?
    (->sentence? #\:)
    (sentence "#char" '(colon))))

(check
  (equal?
    (->sentence? (box 10))
    (sentence "#box" '(10))))

(check
  (equal?
    (->sentence? (bytevector))
    (sentence "#bytevector" '())))

(check
  (equal?
    (->sentence? (bytevector 1 2 3))
    (sentence "#bytevector" '(1 2 3))))

(check
  (equal?
    (->sentence? (vector))
    (sentence "#vector" '())))

(check
  (equal?
    (->sentence? (vector #\a #\space "foo"))
    (sentence "#vector" '(#\a #\space "foo"))))
