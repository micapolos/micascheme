(import (scheme) (lets) (check) (syntax) (switch) (generate) (syntax-match) (boolean))

(define-keywords foo bar)

(define-pattern-match? str
  (syntax-rules ()
    ((_ expr (_ s) body)
      (switch? (syntax->datum expr)
        ((string? $string)
          (lets
            (s $string)
            body))))))

(define-pattern-match? (nums n-1 n n+1) (expr body)
  (switch? (syntax->datum expr)
    ((number? $number)
      (lets
        (n-1 (- $number 1))
        (n $number)
        (n+1 (+ $number 1))
        body))))

; === pattern-match? ===

(check-equal? (pattern-match? #'(_ 1 2) _ "ok") "ok")
(check-equal? (pattern-match? #'(_ 1 2) _ #f) #f)

(check-equal? (pattern-match? #'(+ 1 2) #'x (syntax->datum x)) '(+ 1 2))

(check-equal? (pattern-match? #'() () "ok") "ok")
(check-equal? (pattern-match? #'128 128 "ok") "ok")
(check-equal? (pattern-match? #'"foo" "foo" "ok") "ok")
(check-equal? (pattern-match? #'#\a #\a "ok") "ok")
(check-equal? (pattern-match? #'#t #t "ok") "ok")
(check-equal? (pattern-match? #'#f #f "ok") "ok")
(check-equal? (pattern-match? #'#f #f #f) #f)

(check (false? (pattern-match? #'() 123 "ok")))
(check (false? (pattern-match? #'123 () "ok")))

(check-equal? (pattern-match? #'foo foo "ok") "ok")
(check (false? (pattern-match? #'bar foo "ok")))

(check-equal? (pattern-match? #'(foo bar) (foo bar) "ok") "ok")
(check (false? (pattern-match? #'(foo foo) (foo bar) "ok")))
(check (false? (pattern-match? #'(bar bar) (foo bar) "ok")))

(check-equal?
  (pattern-match?
    #'((+ 1 2))
    (#'x)
    (syntax->datum x))
  '(+ 1 2))

(check-equal?
  (pattern-match?
    #'((+ 1 2) (* 3 4))
    (#'x #'y)
    (list (syntax->datum x) (syntax->datum y)))
  '((+ 1 2) (* 3 4)))

(check-equal?
  (pattern-match?
    #'"foo"
    (str s)
    (string-append s "!"))
  "foo!")

(check
  (false?
    (pattern-match? #'123 (str s) (string-append s "!"))))

(check-equal?
  (pattern-match?
    #'10
    (nums $n-1 $n $n+1)
    (list $n-1 $n $n+1))
  (list 9 10 11))

(check
  (false?
    (pattern-match?
      #'"foo"
      (nums $n-1 $n $n+1)
      (list $n-1 $n $n+1))))

; === syntax-match? ===

(check
  (false?
    (syntax-match? #'1)))

(check-equal?
  (syntax-match? #'1
    (1 "one"))
  "one")

(check
  (false?
    (syntax-match? #'2
      (1 "one"))))

(check-equal?
  (syntax-match? #'2
    (1 "one")
    (2 "two"))
  "two")

(check-equal?
  (syntax-match? #'foo
    (foo "foo"))
  "foo")

(check-equal?
  (syntax-match? #'(foo bar)
    ((foo bar) "ok"))
  "ok")

(check
  (false?
    (syntax-match? #'(foo foo)
      ((foo bar) "ok"))))

(check-equal?
  (syntax-match? #'(foo bar bar)
    ((foo _ _) "ok")
    (_ #f))
  "ok")

(check-equal?
  (syntax-match? #'(+ 1 2)
    (#'x (syntax->datum x)))
  '(+ 1 2))

(check-equal?
  (syntax-match? #'(+ 1 2)
    ((#'x #'y #'z)
      (list (syntax->datum x) (syntax->datum y) (syntax->datum z))))
  '(+ 1 2))

(check-equal?
  (syntax-match? #'"foo"
    ((str s) (string-append s "!")))
  "foo!")

; === syntax-match ===

(check-equal?
  (syntax-match #'"foo"
    ((str s) (string-append s "!")))
  "foo!")

(check
  (raises
    (syntax-match #'123
      ((str s) (string-append s "!")))))
