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

(check (equal? (term->datum (conditional `cond `true `false)) `(if 'cond 'true 'false)))

(check (equal? (term->datum (tuple-type `foo (list))) `(tuple-type 'foo (list))))
(check (equal? (term->datum (tuple-type `foo (list `t1 `t2))) `(tuple-type 'foo (list 't1 't2))))

(check (equal? (term->datum (universe 3)) `(universe 3)))

(check (equal? (term->datum (function-type `foo (list `t1 `t2) `t3)) `(function-type 'foo (list 't1 't2) 't3)))

(check (equal? (term->datum (function 2 `foo)) `(lambda (v0 v1) 'foo)))
(check (equal? (term->datum (function 2 (variable 0))) `(lambda (v0 v1) v1)))
(check (equal? (term->datum (function 2 (variable 1))) `(lambda (v0 v1) v0)))

(check (equal? (term->datum (application `foo (list `bar `goo))) `('foo 'bar 'goo)))

(check (equal? (term->datum (pair `p1 `p2)) `(cons 'p1 'p2)))
(check (equal? (term->datum (pair-first `pair)) `(car 'pair)))
(check (equal? (term->datum (pair-second `pair)) `(cdr 'pair)))

(check (equal? (term->datum (tuple!)) #f))
(check (equal? (term->datum (tuple! `t1)) ''t1))
(check (equal? (term->datum (tuple! `t1 `t2)) `(cons 't1 't2)))
(check (equal? (term->datum (tuple! `t1 `t2 `t3)) `(vector 't1 't2 't3)))

(check (equal? (term->datum (tuple-ref 1 `tuple 0)) ''tuple))
(check (equal? (term->datum (tuple-ref 2 `tuple 0)) `(car 'tuple)))
(check (equal? (term->datum (tuple-ref 2 `tuple 1)) `(cdr 'tuple)))
(check (equal? (term->datum (tuple-ref 3 `tuple 0)) `(vector-ref 'tuple 0)))
(check (equal? (term->datum (tuple-ref 3 `tuple 1)) `(vector-ref 'tuple 1)))
(check (equal? (term->datum (tuple-ref 3 `tuple 2)) `(vector-ref 'tuple 2)))

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
          (pair "zero" (variable 0)))))
    `(let ((v0 'foo)) 
      (cons "zero" v0))))

(check 
  (equal? 
    (term->datum 
      (choice-switch 2 `foo 
        (list 
          (pair "zero" (variable 0))
          (pair "one" (variable 0)))))
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
          (pair "zero" (variable 0))
          (pair "one" (variable 0))
          (pair "two" (variable 0)))))
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
