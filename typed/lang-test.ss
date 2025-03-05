(import (micascheme) (typed lang) (typed type))

(define-typed x 123)
(define-typed fixnum+ (assume (any-lambda (any-fixnum any-fixnum) any-fixnum) fx+))
(define-typed string+ (assume (any-lambda (any-string any-string) any-string) string-append))

(check (equal? (tt (assume 'type 'value)) 'value))
(check (equal? (tt (: any-fixnum 123)) 123))
(check (equal? (tt 123) 123))
(check (equal? (tt ((lambda () 123))) 123))
(check (equal? (tt ((lambda ((any-fixnum fx)) fx) 123)) 123))
(check (equal? (tt x) 123))
(check (equal? (tt (fixnum+ x 1)) 124))
(check (equal? (tt (string+ "foo" "bar")) "foobar"))
