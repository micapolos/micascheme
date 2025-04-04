(import (micascheme) (typed lang) (typed type) (typed phased) (typed typed))

(check (equal? (tt (assume any-string "foo")) "foo"))

(check (equal? (tt any-boolean) any-boolean))
(check (equal? (tt (typeof any-boolean)) (any any-boolean)))

(check (equal? (tt any-number) any-number))
(check (equal? (tt (typeof any-number)) (any any-number)))

(check (equal? (tt any-string) any-string))
(check (equal? (tt (typeof any-string)) (any any-string)))

(check (equal? (tt any-syntax) any-syntax))
(check (equal? (tt (typeof any-syntax)) (any any-syntax)))

(check
  (equal?
    (tt (any-lambda (any-string any-number) any-boolean))
    (any-lambda (any-string any-number) any-boolean)))

(check
  (equal?
    (tt (typeof (any-lambda (any-string any-number) any-boolean)))
    any-any-lambda))

(check (equal? (tt #f) #f))
(check (equal? (tt (typeof #f)) any-boolean))

(check (equal? (tt 123) 123))
(check (equal? (tt (typeof 123)) any-number))

(check (equal? (tt "foo") "foo"))
(check (equal? (tt (typeof "foo")) any-string))

(check (syntax=? (tt #'(foo bar)) #'(foo bar)))
(check (syntax=? (tt (typeof #'(foo bar))) any-syntax))

(check (equal? ((tt (lambda () "foo"))) "foo"))
(check (equal? (tt (typeof (lambda () "foo"))) (any-lambda () any-string)))

(check (equal? ((tt (lambda ((any-string s)) s)) "foo") "foo"))
(check (equal? (tt (typeof (lambda ((any-string s)) s))) (any-lambda (any-string) any-string)))

(check (equal? (tt (typeof (typeof "foo"))) any-type))
(check (equal? (tt (typeof (typeof (typeof "foo")))) any-type))

(tt (define foo "foo"))

(check (equal? (tt foo) "foo"))
(check (equal? (tt (typeof foo)) any-string))

(tt (define (same-string (any-string s)) s))

(check (equal? (tt (typeof same-string)) (any-lambda (any-string) any-string)))
(check (equal? ((tt same-string) "foo") "foo"))

(tt (define string+ (assume (any-lambda (any-string any-string) any-string) string-append)))

(check (equal? (tt (typeof string+)) (any-lambda (any-string any-string) any-string)))
(check (equal? (tt (string+ "foo" "bar")) "foobar"))
