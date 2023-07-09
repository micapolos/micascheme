(import (micascheme) (variable) (term))

; === utils ===

(check 
  (obj=? 
    (use (list "foo" 128) (variable 0)) 
    (application (function 2 (variable 0)) (list "foo" 128))))

; === term->datum ===

(define-syntax-rule (check-datum $term $datum)
  (check (equal? (term->datum $term) (quote $datum))))

(check-datum (native `string-append) string-append)

(check-datum `foo 'foo)

(check-datum #f #f)
(check-datum #t #t)
(check-datum 123 123)
(check-datum "foo" "foo")

(check-datum (boolean-type) (boolean-type))
(check-datum (number-type) (number-type))
(check-datum (string-type) (string-type))

(check-datum (conditional `cond `true `false) (if 'cond 'true 'false))

(check-datum (tuple-type `foo (list)) (tuple-type 'foo (list)))
(check-datum (tuple-type `foo (list `t1 `t2)) (tuple-type 'foo (list 't1 't2)))

(check-datum (universe 3) (universe 3))

(check-datum (function-type `foo (list `t1 `t2) `t3) (function-type 'foo (list 't1 't2) 't3))

(check-datum (function 2 `foo) (lambda (v0 v1) 'foo))
(check-datum (function 2 (variable 0)) (lambda (v0 v1) v1))
(check-datum (function 2 (variable 1)) (lambda (v0 v1) v0))

(check-datum (application `foo (list `bar `goo)) ('foo 'bar 'goo))

(check-datum (ordinal 1 0) #f)
(check-datum (ordinal 2 0) #t)
(check-datum (ordinal 2 1) #f)
(check-datum (ordinal 3 0) 0)
(check-datum (ordinal 3 1) 1)
(check-datum (ordinal 3 2) 2)

(check-datum (ordinal-switch! `ordinal `v0) 'v0)
(check-datum (ordinal-switch! `ordinal `v0 `v1) (if 'ordinal 'v0 'v1))
(check-datum (ordinal-switch! `ordinal `v0 `v1 `v2) (index-switch 'ordinal 'v0 'v1 'v2))

(check-datum (pair `a `b) (cons 'a 'b))
(check-datum (pair-first `pair) (car 'pair))
(check-datum (pair-second `pair) (cdr 'pair))

(check-datum (vector `a `b) (vector 'a 'b))
(check-datum (vector-get `vector `index) (vector-ref 'vector 'index))

(check-datum (tuple!) #f)
(check-datum (tuple! `t1) 't1)
(check-datum (tuple! `t1 `t2) (cons 't1 't2))
(check-datum (tuple! `t1 `t2 `t3) (vector 't1 't2 't3))

(check-datum (tuple-ref 1 `tuple 0) 'tuple)
(check-datum (tuple-ref 2 `tuple 0) (car 'tuple))
(check-datum (tuple-ref 2 `tuple 1) (cdr 'tuple))
(check-datum (tuple-ref 3 `tuple 0) (vector-ref 'tuple 0))
(check-datum (tuple-ref 3 `tuple 1) (vector-ref 'tuple 1))
(check-datum (tuple-ref 3 `tuple 2) (vector-ref 'tuple 2))

; === term-eval ===

(define-syntax-rule (check-eval $term $value)
  (check (equal? (term-eval $term (environment `(micascheme))) $value)))

(check-eval (application! (native `string-append) "Hello, " "world!") "Hello, world!")

(check-eval (ordinal 3 0) 0)
(check-eval (ordinal-switch! (ordinal 3 0) "one" "two" "three") "one")

(check-eval 
  (application!
    (function 1
      (application!
        (function 1 (ordinal-switch! (pair-first v1) "boolean" "number" v0))
        (pair-second v0)))
    (cons (ordinal 3 1) "foo"))
  "number")

(check-eval 
  (application!
    (function 1
      (application!
        (function 1 (ordinal-switch! (pair-first v1) "boolean" "number" v0))
        (pair-second v0)))
    (cons (ordinal 3 2) "foo"))
  "foo")
