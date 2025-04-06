(import (scheme) (check) (match) (syntax))

(check (equal? (match-case? #t (#t "true") ($other $other)) "true"))
(check (equal? (match-case? #f (#t "true") ($other $other)) #f))

(check (equal? (match-case? #\a (#\a "a") ($other $other)) "a"))
(check (equal? (match-case? #\b (#\a "a") ($other $other)) #\b))

(check (equal? (match-case? 128 (128 "128") ($other $other)) "128"))
(check (equal? (match-case? 129 (128 "128") ($other $other)) 129))

(check (equal? (match-case? "foo" ("foo" "foo!") ($other $other)) "foo!"))
(check (equal? (match-case? "bar" ("foo" "foo!") ($other $other)) "bar"))

(check (equal? (match-case? "foo" (foo (string-append foo "!"))) "foo!"))

(define-predicate-matcher string?)
(define-predicate-matcher number?)

(define-property cons match-prim?
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ val (_ a b) body)
        #`(let ((a (car val)) (b (cdr val)))
            body)))))

(check (equal? (if-matches "foo" (string? s) (string-append s "!") "error") "foo!"))
(check (equal? (if-matches 123 (string? s) (string-append s "!") "error") "error"))

(check
  (equal?
    (match-case? "foo"
      ((string? s) (string-append s "!"))
      ((number? n) (number->string n)))
    "foo!"))

(check
  (equal?
    (match-case? 123
      ((string? s) (string-append s "!"))
      ((number? n) (+ n 1)))
    124))

(check
  (equal?
    (match-case? #\a
      ((string? s) (string-append s "!"))
      ((number? n) (+ n 1)))
    #f))

(check
  (equal?
    (match-case? #\a
      ((string? s) (string-append s "!"))
      ((number? n) (+ n 1))
      (x (format "char ~a" x)))
    "char a"))

(check
  (equal?
    (matcher (cons 1 2) (cons a b) (cons b a))
    (cons 2 1)))

(check
  (equal?
    (matcher (list 1 2) (cons a (cons b c)) (cons b (cons a c)))
    (list 2 1)))
