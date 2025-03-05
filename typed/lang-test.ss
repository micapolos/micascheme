(import (micascheme) (typed lang) (typed type))

(check (equal? (tt (assume 'type 'value)) 'value))
(check (equal? (tt (: any-fixnum 123)) 123))
(check (equal? (tt 123) 123))
(check (equal? (tt ((lambda () 123))) 123))
(check (equal? (tt ((lambda ((any-fixnum fx)) fx) 123)) 123))

(define-typed x 10)
(check (equal? (tt x) 10))

(define-typed fixnum+ (assume (any-lambda (any-fixnum any-fixnum) any-fixnum) fx+))
(define-typed y (fixnum+ x 1))
(check (equal? (tt y) 11))

(define-typed foo "foo")
(check (equal? (tt foo) "foo"))

(define-typed string+ (assume (any-lambda (any-string any-string) any-string) string-append))
(define-typed foobar (string+ foo "bar"))
(check (equal? (tt foobar) "foobar"))
