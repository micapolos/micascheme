(import (scheme) (check) (throw) (syntax))

(check (raises (throw error)))
(check (works 123))

(define-check-> string)

(check-string (list (list #\a #\b)) "ab")
(check-string (number 123) "123")
(check-string (raises (list #\a #\b 1)))

(define-check-datum-> foo)
(define-aux-keyword bar)

(define (bar->foo $bar)
  (syntax-case $bar (bar)
    ((bar x) #`(foo x))))

(check-foo (bar (bar 10)) (foo 10))
(check-foo (raises (bar (non-bar 10))))
