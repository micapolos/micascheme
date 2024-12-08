(import (scheme) (lets) (check) (syntax) (generate) (syntax-match))

(define-aux-keywords foo bar)

(define-syntax-match-clause string-match
  (lambda ($syntax)
    (syntax-case $syntax ()
      (((_ s) body)
        #`(s
          (string? (datum s))
          (let ((s (datum s))) body))))))

(define-syntax-match-clause (number-match n) body
  (n
    (number? (datum n))
    (let ((n (datum n))) body)))

; === syntax-match ===

(check
  (raises
    (syntax-match #'1)))

(check
  (equal?
    (syntax-match #'1
      (_ #f))
    #f))

(check-equal?
  (syntax-match #'1
    (1 "one")
    (_ #f))
  "one")

(check-equal?
  (syntax-match #'2
    (1 "one")
    (_ #f))
  #f)

(check-equal?
  (syntax-match #'2
    (1 "one")
    (2 "two")
    (_ #f))
  "two")

(check-equal?
  (syntax-match #'foo
    (foo "foo")
    (_ #f))
  "foo")

(check-equal?
  (syntax-match #'(foo bar)
    ((foo bar) "ok"))
  "ok")

(check-equal?
  (syntax-match #'(foo foo)
    ((foo bar) "ok")
    (_ #f))
  #f)

(check-equal?
  (syntax-match #'(+ 1 2)
    (#'x (datum x)))
  '(+ 1 2))

(check-equal?
  (syntax-match #'"foo"
    ((string-match s) (string-append s "!")))
  "foo!")

(check-equal?
  (syntax-match #'123
    ((number-match n) (+ n 1)))
  124)

(check
  (raises
    (syntax-match #'123
      ((string-match s) (string-append s "!")))))

; === syntax-match? ===

(check-equal?
  (syntax-match? #'"foo"
    ((string-match s) (string-append s "!")))
  "foo!")

(check-equal?
  (syntax-match? #'123
    ((string-match s) (string-append s "!")))
  #f)

; === multi match ===

(define-syntax-match-clause (numbers-match n-1 n n+1) body
  (n
    (number? (datum n))
    (lets
      (n (datum n))
      (n+1 (+ n 1))
      (n-1 (- n 1))
      body)))

(check-equal?
  (syntax-match #'10
    ((numbers-match a b c) (list a b c)))
  (list 9 10 11))
