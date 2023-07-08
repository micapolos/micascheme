(import (micascheme) (term))

; === term->datum ===

(check (equal? (term->datum (native `string-append)) `string-append))

(check (equal? (term->datum `foo) ''foo))

(check (equal? (term->datum #f) #f))
(check (equal? (term->datum #t) #t))
(check (equal? (term->datum 123) 123))
(check (equal? (term->datum "foo") "foo"))

(check (equal? (term->datum (boolean-type)) `(boolean-type)))
(check (equal? (term->datum (number-type)) `(number-type)))
(check (equal? (term->datum (string-type)) `(string-type)))

(check (equal? (term->datum (tuple-type `foo (list))) `(tuple-type 'foo (list))))
(check (equal? (term->datum (tuple-type `foo (list `t1 `t2))) `(tuple-type 'foo (list 't1 't2))))

(check (equal? (term->datum (universe 3)) `(universe 3)))

(check (equal? (term->datum (function-type `foo (list `t1 `t2) `t3)) `(function-type 'foo (list 't1 't2) 't3)))

(check (equal? (term->datum (function 2 `foo)) `(lambda (v0 v1) 'foo)))
(check (equal? (term->datum (function 2 (variable 0))) `(lambda (v0 v1) v1)))
(check (equal? (term->datum (function 2 (variable 1))) `(lambda (v0 v1) v0)))

(check (equal? (term->datum (application `foo (list `bar `goo))) `('foo 'bar 'goo)))

(check (equal? (term->datum (tuple! (foo))) #f))
(check (equal? (term->datum (tuple! (foo (typed "foo" string!)))) "foo"))
(check (equal? (term->datum (tuple! (foo (typed "foo" string!) (typed "bar" string!)))) `(cons "foo" "bar")))
(check (equal? (term->datum (tuple! (foo (typed "foo" string!) (typed "bar" string!) (typed "goo" string!)))) `(vector "foo" "bar" "goo")))

(check (equal? (term->datum (tuple-ref (typed `x (tuple-type! (foo number!))) 0)) ''x))
(check (equal? (term->datum (tuple-ref (typed `x (tuple-type! (foo number! string!))) 0)) `(car 'x)))
(check (equal? (term->datum (tuple-ref (typed `x (tuple-type! (foo number! string!))) 1)) `(cdr 'x)))
(check (equal? (term->datum (tuple-ref (typed `x (tuple-type! (foo number! string! boolean!))) 0)) `(vector-ref 'x 0)))
(check (equal? (term->datum (tuple-ref (typed `x (tuple-type! (foo number! string! boolean!))) 1)) `(vector-ref 'x 1)))
(check (equal? (term->datum (tuple-ref (typed `x (tuple-type! (foo number! string! boolean!))) 2)) `(vector-ref 'x 2)))

; === select

(check (equal? (term->datum (select 1 0 "foo")) "foo"))
(check (equal? (term->datum (select 2 0 "foo")) `(cons #f "foo")))
(check (equal? (term->datum (select 2 1 "foo")) `(cons #t "foo")))
(check (equal? (term->datum (select 3 0 "foo")) `(cons 0 "foo")))
(check (equal? (term->datum (select 3 1 "foo")) `(cons 1 "foo")))
(check (equal? (term->datum (select 3 2 "foo")) `(cons 2 "foo")))

; === choice-switch

(check 
  (equal? 
    (term->datum 
      (choice-switch 1 `foo 
        (list 
          (tuple!
            (foo 
              (typed "zero" string!) 
              (typed (variable 0) string!))))))
    `(let ((v0 'foo)) 
      (cons "zero" v0))))

(check 
  (equal? 
    (term->datum 
      (choice-switch 2 `foo 
        (list 
          (tuple! 
            (foo 
              (typed "zero" string!) 
              (typed (variable 0) string!)))
          (tuple! 
            (foo 
              (typed "one" string!) 
              (typed (variable 0) string!))))))
    `(let ((v0 'foo)) 
      (let ((v1 (cdr v0))) 
        (if (car v0) 
          (cons "zero" v1) 
          (cons "one" v1))))))

(check 
  (equal? 
    (term->datum 
      (choice-switch 3 `foo 
        (list 
          (tuple! 
            (foo 
              (typed "zero" string!) 
              (typed (variable 0) string!)))
          (tuple! 
            (foo 
              (typed "one" string!) 
              (typed (variable 0) string!)))
          (tuple! 
            (foo 
              (typed "two" string!) 
              (typed (variable 0) string!))))))
    `(let ((v0 'foo)) 
      (let ((v1 (cdr v0))) 
        (index-switch (car v0) 
          (cons "zero" v1) 
          (cons "one" v1) 
          (cons "two" v1))))))

; === eval-term ===

(check
  (equal?
    (eval-term
      (application! (native `string-append) "Hello, " "world!")
      (environment `(micascheme)))
    "Hello, world!"))
