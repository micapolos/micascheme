(import (micascheme) (typed lang) (typed type) (typed phased) (typed typed))

(check (equal? (tt any-boolean) any-boolean))
(check (equal? (tt any-number) any-number))
(check (equal? (tt any-string) any-string))
(check (equal? (tt any-syntax) any-syntax))

(check
  (equal?
    (tt (any-lambda (any-string any-number) any-boolean))
    (any-lambda (any-string any-number) any-boolean)))

(check (equal? (tt #f) #f))
(check (equal? (tt 123) 123))
(check (equal? (tt "foo") "foo"))
(check (syntax=? (tt #'(foo bar)) #'(foo bar)))

(define-phased foo (phased 0 (typed any-string #'"foo")))

(check (equal? (tt foo) "foo"))

(check (equal? ((tt (lambda () "foo"))) "foo"))
(check (equal? ((tt (lambda ((any-string s)) s)) "foo") "foo"))
