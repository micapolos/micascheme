(import (chezscheme) (base))

; === bind ===

(check (equal? (lets 1) 1))
(check (equal? (lets (x 1) (y (+ x 2)) y) 3))

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

; === indexed ===

(check (equal? (indexed-value (indexed "a" 1)) "a"))
(check (equal? (indexed-index (indexed "a" 1)) 1))

; === map-indexed ===

(check (equal? (map-indexed cons (list "a" "b" "c")) (list (cons 0 "a") (cons 1 "b") (cons 2 "c"))))

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
    (syntax->datum (struct->datum-syntax #`foo (list) generate-test-temporary))
    `(define (foo->datum $expr) `(foo))))

(check 
  (equal? 
    (syntax->datum (struct->datum-syntax #`foo (list #`string) generate-test-temporary))
    `(define (foo->datum $expr) 
      `(foo 
        ,(string->datum (foo-string $expr))))))

(check 
  (equal? 
    (syntax->datum (struct->datum-syntax #`foo (list #`string #`number) generate-test-temporary))
    `(define (foo->datum $expr) 
      `(foo 
        ,(string->datum (foo-string $expr))
        ,(number->datum (foo-number $expr))))))

(check 
  (equal? 
    (syntax->datum (struct->datum-syntax #`foo (list #`string #`number #`bar) generate-test-temporary))
    `(define (foo->datum $expr) 
      `(foo 
        ,(string->datum (foo-string $expr))
        ,(number->datum (foo-number $expr))
        ,(bar->datum (foo-bar $expr))))))

; === struct->syntax ===

(check 
  (equal? 
    (syntax->datum (struct-syntax #`foo (list #`string #`number) generate-test-temporary))
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
    (syntax->datum (one-of-constructor-syntax #`foo (list #`string) generate-test-temporary))
    `(define-syntax foo 
      (syntax-rules (not string) 
        ((_ $string) (cons 0 $string))))))

(check 
  (equal? 
    (syntax->datum (one-of-constructor-syntax #`foo (list #`string #`number) generate-test-temporary))
    `(define-syntax foo 
      (syntax-rules (not string number) 
        ((_ $string (not number)) (cons 0 $string)) 
        ((_ (not string) $number) (cons 1 $number))))))

(check 
  (equal? 
    (syntax->datum (one-of-constructor-syntax #`foo (list #`string #`number #`bar) generate-test-temporary))
    `(define-syntax foo 
      (syntax-rules (not string number bar) 
        ((_ $string (not number) (not bar)) (cons 0 $string)) 
        ((_ (not string) $number (not bar)) (cons 1 $number)) 
        ((_ (not string) (not number) $bar) (cons 2 $bar))))))

