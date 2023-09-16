(import (micascheme) (term))

; === utils ===

(check 
  (equal?
    (use (list "foo" 128) (variable 0)) 
    (application (function 2 (variable 0)) (list "foo" 128))))

(check (equal? v0 (variable 0)))
(check (equal? v1 (variable 1)))
(check (equal? v2 (variable 2)))

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

(check-datum (function 2 `foo) (lambda ($v0 $v1) 'foo))
(check-datum (function 2 (variable 0)) (lambda ($v0 $v1) $v1))
(check-datum (function 2 (variable 1)) (lambda ($v0 $v1) $v0))

(check-datum (recursive (function 2 `foo)) (rec $v0 (lambda ($v1 $v2) 'foo)))
(check-datum (recursive (function 2 (variable 0))) (rec $v0 (lambda ($v1 $v2) $v2)))
(check-datum (recursive (function 2 (variable 1))) (rec $v0 (lambda ($v1 $v2) $v1)))
(check-datum (recursive (function 2 (variable 2))) (rec $v0 (lambda ($v1 $v2) $v0)))

(check-datum (application `foo (list `bar `goo)) ('foo 'bar 'goo))

(check-datum (branch! `index `v0) (index-switch 'index 'v0))
(check-datum (branch! `index `v0 `v1) (index-switch 'index 'v0 'v1))
(check-datum (branch! `index `v0 `v1 `v2) (index-switch 'index 'v0 'v1 'v2))

(check-datum (pair `a `b) (cons 'a 'b))
(check-datum (pair-first `pair) (car 'pair))
(check-datum (pair-second `pair) (cdr 'pair))

(check-datum (vector `a `b) (vector 'a 'b))
(check-datum (vector-get `vector `index) (vector-ref 'vector 'index))

; === term-eval ===

(define-syntax-rule (check-eval $term $value)
  (check (equal? (term-eval $term (environment `(micascheme))) $value)))

(check-eval (application! (native `string-append) "Hello, " "world!") "Hello, world!")

(check-eval (branch! 0 "one" "two" "three") "one")
(check-eval (branch! 1 "one" "two" "three") "two")
(check-eval (branch! 2 "one" "two" "three") "three")

(check-eval 
  (use! (cons 1 "foo")
    (use! (pair-second v0)
      (branch! (pair-first v1) "boolean" "number" v0)))
  "number")

(check-eval 
  (use! (cons 2 "foo")
    (use! (pair-second v0)
      (branch! (pair-first v1) "boolean" "number" v0)))
  "foo")

