(import (scheme) (check) (match) (syntax))

(check (equal? (match #t (#t "true") ($other $other)) "true"))
(check (equal? (match #f (#t "true") ($other $other)) #f))

(check (equal? (match #\a (#\a "a") ($other $other)) "a"))
(check (equal? (match #\b (#\a "a") ($other $other)) #\b))

(check (equal? (match 128 (128 "128") ($other $other)) "128"))
(check (equal? (match 129 (128 "128") ($other $other)) 129))

(check (equal? (match "foo" ("foo" "foo!") ($other $other)) "foo!"))
(check (equal? (match "bar" ("foo" "foo!") ($other $other)) "bar"))

(check (equal? (match "foo" (foo (string-append foo "!"))) "foo!"))

(define-predicate-matcher string?)
(define-predicate-matcher number?)

(define-property cons matcher
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ expr (_ a b) body)
        #`(let ((tmp expr))
          (let ((a (car tmp)) (b (cdr tmp)))
            body))))))

(check (equal? (if-matches "foo" (string? s) (string-append s "!") "error") "foo!"))
(check (equal? (if-matches 123 (string? s) (string-append s "!") "error") "error"))

(check
  (equal?
    (match "foo"
      ((string? s) (string-append s "!"))
      ((number? n) (number->string n)))
    "foo!"))

(check
  (equal?
    (match 123
      ((string? s) (string-append s "!"))
      ((number? n) (+ n 1)))
    124))

(check
  (equal?
    (match #\a
      ((string? s) (string-append s "!"))
      ((number? n) (+ n 1)))
    #f))

(check
  (equal?
    (match #\a
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
