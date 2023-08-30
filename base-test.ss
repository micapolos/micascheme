(import (scheme) (base))

; === from ===

(check (equal? (from (base-from-testing) foo) "foo"))
(check (equal? (from (base-from-testing) single?) "single? override"))

; === single ===

(check (equal? (single? (list)) #f))
(check (equal? (single? (list "a")) #t))
(check (equal? (single? (list "a" "b")) #f))

(check (equal? (single (list)) #f))
(check (equal? (single (list "a")) "a"))
(check (equal? (single (list "a" "b")) #f))

; === list-safe-ref ===

(check (obj=? (list-get (list "a" "b") 0) "a"))
(check (obj=? (list-get (list "a" "b") 1) "b"))
(check (obj=? (list-get (list "a" "b") 2) (list-get-overflow 0)))
(check (obj=? (list-get (list "a" "b") 3) (list-get-overflow 1)))

; === lets ===

(check (equal? (lets 1) 1))

(check (equal? (lets (x 1) x) 1))
(check (equal? (lets ((x) 1) x) 1))

(check (equal? (lets (() (values)) 1) 1))
(check (equal? (lets ((x) (values 1)) x) 1))
(check (equal? (lets ((x y) (values 3 2)) (- x y)) 1))

(check
  (equal?
    (lets
      (x 1)
      (y (+ x 2))
      y)
    3))

(check
  (equal?
    (lets
      (x (box 1))
      (do (set-box! x 2))
      (unbox x))
    2))

(check
  (equal?
    (lets
      (fib (rec (lambda (n) (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1)))))))
      (fib 10))
    55))

; === script ===

(check (equal? (script "foo") "foo"))
(check (equal? (script "foo" (string-length)) 3))
(check (equal? (script "foo" (string-append "bar")) "foobar"))
(check (equal? (script "foo" (string-append "bar")) "foobar"))
(check (equal? (script "foo" (string-append "bar") (string-append "goo")) "foobargoo"))
(check (equal? (script "foo" (string-append "bar" "goo")) "foobargoo"))

; === build-identifier ===

(check (equal? (syntax->datum (build-identifier (s #`dupa) (string-append s "-jasiu"))) `dupa-jasiu))

; === associ ===

(check (equal? (associ (list) 100 `a) #f))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `a) (cons 100 `foo)))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `b) (cons 101 `bar)))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `c) #f))

; === map-find-indexed ===

(let ((fn (lambda (s) (and (> (string-length s) 3) (string-append s "!")))))
  (check (obj=? (map-find-indexed fn (list "ala" "ma" "kocicę" "Lunę")) (indexed "kocicę!" 2)))
  (check (obj=? (map-find-indexed fn (list "ala" "ma" "ul")) #f)))

; === curry ===

(check (equal? ((partial string-append "a" "b") "c" "d") "abcd"))

; === indices ===

(check (equal? (indices 3) (list 0 1 2)))

; === filter-map ===

(check (equal? (filter-map (lambda (a b) (and a b)) (list #f #t #f #t) (list 1 2 3 4)) (list 2 4)))

; === indexed ===

(check (equal? (indexed-value (indexed "a" 1)) "a"))
(check (equal? (indexed-index (indexed "a" 1)) 1))

; === map-indexed ===

(check (equal? (map-indexed cons (list "a" "b" "c")) (list (cons 0 "a") (cons 1 "b") (cons 2 "c"))))

; === list-ref-opt ===

(check (equal? (list-ref-opt (list "a" "b") 0) "a"))
(check (equal? (list-ref-opt (list "a" "b") 1) "b"))
(check (equal? (list-ref-opt (list "a" "b") 2) #f))

; === list-indexed ===

(check (obj=? (list-indexed (list "a" "b" "c")) (list (indexed "a" 0) (indexed "b" 1) (indexed "c" 2))))

; === iterate ===

(let (($fn (lambda (s) (string-append s "!"))))
  (check (equal? (iterate $fn "Hello" 0) "Hello"))
  (check (equal? (iterate $fn "Hello" 3) "Hello!!!")))

; === struct-constructor-syntax ===

(check 
  (equal? 
    (syntax->datum (struct-constructor-syntax #`foo (list)))
    `(define-syntax-rule (foo) #f)))

(check 
  (equal? 
    (syntax->datum (struct-constructor-syntax #`foo (list #`string)))
    `(define-syntax-rule (foo string) string)))

(check 
  (equal? 
    (syntax->datum (struct-constructor-syntax #`foo (list #`string #`number)))
    `(define-syntax-rule (foo string number) 
      (cons string number))))

(check 
  (equal? 
    (syntax->datum (struct-constructor-syntax #`foo (list #`string #`number #`bar)))
    `(define-syntax-rule (foo string number bar) 
      (vector string number bar))))

; === struct-accessor-syntax ===

(check 
  (equal? 
    (syntax->datum (struct-accessor-syntax #`foo (list #`string) 0))
    `(define-syntax-rule (foo-string expr) expr)))

(check 
  (equal? 
    (syntax->datum (struct-accessor-syntax #`foo (list #`string #`number) 0))
    `(define-syntax-rule (foo-string expr) (car expr))))

(check 
  (equal? 
    (syntax->datum (struct-accessor-syntax #`foo (list #`string #`number) 1))
    `(define-syntax-rule (foo-number expr) (cdr expr))))

(check 
  (equal? 
    (syntax->datum (struct-accessor-syntax #`foo (list #`string #`number #`bar) 0))
    `(define-syntax-rule (foo-string expr) (vector-ref expr 0))))

(check 
  (equal? 
    (syntax->datum (struct-accessor-syntax #`foo (list #`string #`number #`bar) 1))
    `(define-syntax-rule (foo-number expr) (vector-ref expr 1))))

(check 
  (equal? 
    (syntax->datum (struct-accessor-syntax #`foo (list #`string #`number #`bar) 2))
    `(define-syntax-rule (foo-bar expr) (vector-ref expr 2))))

; === struct->datum-syntax ===

(check 
  (equal? 
    (syntax->datum (struct->datum-syntax #`foo (list)))
    `(define (foo->datum $expr) `(foo))))

(check 
  (equal? 
    (syntax->datum (struct->datum-syntax #`foo (list #`string)))
    `(define (foo->datum $expr) 
      `(foo 
        ,(string->datum (foo-string $expr))))))

(check 
  (equal? 
    (syntax->datum (struct->datum-syntax #`foo (list #`string #`number)))
    `(define (foo->datum $expr) 
      `(foo 
        ,(string->datum (foo-string $expr))
        ,(number->datum (foo-number $expr))))))

(check 
  (equal? 
    (syntax->datum (struct->datum-syntax #`foo (list #`string #`number #`bar)))
    `(define (foo->datum $expr) 
      `(foo 
        ,(string->datum (foo-string $expr))
        ,(number->datum (foo-number $expr))
        ,(bar->datum (foo-bar $expr))))))

; === struct->syntax ===

(check 
  (equal? 
    (syntax->datum (struct-syntax #`foo (list #`string #`number)))
    `(begin 
      (define-syntax-rule (foo string number) (cons string number)) 
      (define-syntax-rule (foo-string expr) (car expr))
      (define-syntax-rule (foo-number expr) (cdr expr)) 
      (define (foo->datum $expr) 
        `(foo 
          ,(string->datum (foo-string $expr))
          ,(number->datum (foo-number $expr)))))))

; === one-of-constructor-syntax ===

(check 
  (equal? 
    (syntax->datum (one-of-constructor-syntax #`foo (list #`string #`number #`bar) 0))
    `(define-syntax-rule (string-foo one-of)
      (cons 0 one-of))))

(check 
  (equal? 
    (syntax->datum (one-of-constructor-syntax #`foo (list #`string #`number #`bar) 1))
    `(define-syntax-rule (number-foo one-of)
      (cons 1 one-of))))

(check 
  (equal? 
    (syntax->datum (one-of-constructor-syntax #`foo (list #`string #`number #`bar) 2))
    `(define-syntax-rule (bar-foo one-of)
      (cons 2 one-of))))

; === one-of-switch-syntax ===

(check 
  (equal? 
    (syntax->datum (one-of-switch-syntax #`foo (list #`string #`number #`bar)))
    `(define-syntax foo-switch 
      (syntax-rules (string? number? bar?) 
        ((_ one-of 
          ((string? $string) string-body) 
          ((number? $number) number-body) 
          ((bar? $bar) bar-body))
        (lets 
          ($one-of one-of) 
          ($index (car $one-of)) 
          ($value (cdr $one-of)) 
          (case $index 
            ((0) (lets ($string $value) string-body)) 
            ((1) (lets ($number $value) number-body)) 
            ((2) (lets ($bar $value) bar-body)))))))))

; === one-of->datum-syntax ===

(check 
  (equal? 
    (syntax->datum (one-of->datum-syntax #`foo (list #`string #`number #`bar)))
    `(define (foo->datum $one-of)
      (foo-switch $one-of
        ((string? $string) `(foo ,(string->datum $string)))
        ((number? $number) `(foo ,(number->datum $number)))
        ((bar? $bar) `(foo ,(bar->datum $bar)))))))

; === one-of-syntax ===

(check 
  (equal? 
    (syntax->datum (one-of-syntax #`foo (list #`string #`number)))
    `(begin 
      (define-syntax-rule (string-foo one-of) (cons 0 one-of)) 
      (define-syntax-rule (number-foo one-of) (cons 1 one-of)) 
      (define-syntax foo-switch (syntax-rules (string? number?) 
        ((_ one-of 
          ((string? $string) string-body) 
          ((number? $number) number-body))
        (lets 
          ($one-of one-of) 
          ($index (car $one-of)) 
          ($value (cdr $one-of)) 
          (case $index 
            ((0) (lets ($string $value) string-body)) 
            ((1) (lets ($number $value) number-body))))))) 
      (define (foo->datum $one-of) 
        (foo-switch $one-of 
          ((string? $string) (quasiquote (foo (unquote (string->datum $string))))) 
          ((number? $number) (quasiquote (foo (unquote (number->datum $number))))))))))

; === stack ===

(check (equal? (stack) (list)))
(check (equal? (stack 1 2 3) (list 3 2 1)))
(check (equal? (top (stack 1 2 3)) 3))
(check (equal? (pop (stack 1 2 3)) (stack 1 2)))
