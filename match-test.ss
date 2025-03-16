(import (scheme) (check) (match) (syntax))

(check (equal? (match "foo" (foo (string-append foo "!"))) "foo!"))

(define-property string matcher
  (syntax-rules ()
    ((_ expr (_ s) body)
      (let ((val expr))
        (and
          (string? val)
          (lambda ()
            (let ((s val)) body)))))))

(define-aux-keyword number)

(define-property number matcher
  (syntax-rules ()
    ((_ expr (_ s) body)
      (let ((val expr))
        (and
          (number? val)
          (lambda ()
            (let ((s val)) body)))))))

(check (equal? (if-matches "foo" (string s) (string-append s "!") "error") "foo!"))
(check (equal? (if-matches 123 (string s) (string-append s "!") "error") "error"))

(check
  (equal?
    (match "foo"
      ((string s) (string-append s "!"))
      ((number n) (number->string n)))
    "foo!"))

(check
  (equal?
    (match 123
      ((string s) (string-append s "!"))
      ((number n) (+ n 1)))
    124))

(check
  (raises
    (match #\a
      ((string s) (string-append s "!"))
      ((number n) (+ n 1)))))

(check
  (equal?
    (match #\a
      ((string s) (string-append s "!"))
      ((number n) (+ n 1))
      (x (format "char ~a" x)))
    "char a"))
