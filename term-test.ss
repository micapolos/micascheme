(import (micascheme) (term))

; === term->datum ===

(check (equal? (term->datum (native `string-append)) `string-append))

(check (equal? (term->datum `foo) ''foo))

(check (equal? (term->datum #f) #f))
(check (equal? (term->datum #t) #t))
(check (equal? (term->datum 123) 123))
(check (equal? (term->datum "foo") "foo"))

(check (equal? (term->datum (any-boolean)) `(any-boolean)))
(check (equal? (term->datum (any-number)) `(any-number)))
(check (equal? (term->datum (any-string)) `(any-string)))

(check (equal? (term->datum (any-tuple `foo (list))) `(any-tuple 'foo (list))))
(check (equal? (term->datum (any-tuple `foo (list `t1 `t2))) `(any-tuple 'foo (list 't1 't2))))

(check (equal? (term->datum (any-type)) `(any-type)))

(check (equal? (term->datum (arrow `t1 `t2)) `(arrow 't1 't2)))

(check (equal? (term->datum (abstraction 2 `foo)) `(lambda (v0 v1) 'foo)))
(check (equal? (term->datum (abstraction 2 (variable 0))) `(lambda (v0 v1) v1)))
(check (equal? (term->datum (abstraction 2 (variable 1))) `(lambda (v0 v1) v0)))

(check (equal? (term->datum (application `foo (list `bar `goo))) `('foo 'bar 'goo)))

(check (equal? (term->datum (tuple!)) #f))
(check (equal? (term->datum (tuple! "foo")) "foo"))
(check (equal? (term->datum (tuple! "foo" "bar")) `(cons "foo" "bar")))
(check (equal? (term->datum (tuple! "foo" "bar" "goo")) `(vector "foo" "bar" "goo")))

(check (equal? (term->datum (tuple-get 1 `x 0)) ''x))
(check (equal? (term->datum (tuple-get 2 `x 0)) `(car 'x)))
(check (equal? (term->datum (tuple-get 2 `x 1)) `(cdr 'x)))
(check (equal? (term->datum (tuple-get 3 `x 0)) `(vector-ref 'x 0)))
(check (equal? (term->datum (tuple-get 3 `x 1)) `(vector-ref 'x 1)))
(check (equal? (term->datum (tuple-get 3 `x 2)) `(vector-ref 'x 2)))

; === eval-term ===

(check
  (equal?
    (eval-term
      (application! (native `string-append) "Hello, " "world!")
      (environment `(micascheme)))
    "Hello, world!"))
