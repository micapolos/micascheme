(import (scheme) (lets) (check) (syntax) (switch) (generate) (syntax-match) (boolean))

(define-aux-keywords foo bar)

(define-pattern-match? str
  (syntax-rules ()
    ((_ expr (_ s) body)
      (switch-opt (syntax->datum expr)
        ((string? $string)
          (lets
            (s $string)
            body))))))

(define-pattern-match? nums
  (syntax-rules ()
    ((_ expr (_ n-1 n n+1) body)
      (switch-opt (syntax->datum expr)
        ((number? $number)
          (lets
            (n-1 (- $number 1))
            (n $number)
            (n+1 (+ $number 1))
            body))))))

; === pattern-match? ===

(check-equal? (pattern-match? #'(_ 1 2) _ (match "ok")) (match "ok"))
(check-equal? (pattern-match? #'(_ 1 2) _ (match #f)) (match #f))
(check-equal? (pattern-match? #'(_ 1 2) _ #f) #f)

(check-equal? (pattern-match? #'(+ 1 2) #'x (match (syntax->datum x))) (match '(+ 1 2)))

(check-equal? (pattern-match? #'() () (match "ok")) (match "ok"))
(check-equal? (pattern-match? #'128 128 (match "ok")) (match "ok"))
(check-equal? (pattern-match? #'"foo" "foo" (match "ok")) (match "ok"))
(check-equal? (pattern-match? #'#\a #\a (match "ok")) (match "ok"))
(check-equal? (pattern-match? #'#t #t (match "ok")) (match "ok"))
(check-equal? (pattern-match? #'#f #f (match "ok")) (match "ok"))
(check-equal? (pattern-match? #'#f #f (match #f)) (match #f))

(check (false? (pattern-match? #'() 123 (match "ok"))))
(check (false? (pattern-match? #'123 () (match "ok"))))

(check-equal? (pattern-match? #'foo foo (match "ok")) (match "ok"))
(check (false? (pattern-match? #'bar foo (match "ok"))))

(check-equal? (pattern-match? #'(foo bar) (foo bar) (match "ok")) (match "ok"))
(check (false? (pattern-match? #'(foo foo) (foo bar) (match "ok"))))
(check (false? (pattern-match? #'(bar bar) (foo bar) (match "ok"))))

(check-equal?
  (pattern-match?
    #'((+ 1 2))
    (#'x)
    (match (syntax->datum x)))
  (match '(+ 1 2)))

(check-equal?
  (pattern-match?
    #'((+ 1 2) (* 3 4))
    (#'x #'y)
    (match (list (syntax->datum x) (syntax->datum y))))
  (match '((+ 1 2) (* 3 4))))

(check-equal?
  (pattern-match?
    #'"foo"
    (str s)
    (match (string-append s "!")))
  (match "foo!"))

(check
  (false?
    (pattern-match? #'123 (str s) (match (string-append s "!")))))

(check-equal?
  (pattern-match?
    #'10
    (nums $n-1 $n $n+1)
    (match (list $n-1 $n $n+1)))
  (match (list 9 10 11)))

(check
  (false?
    (pattern-match?
      #'"foo"
      (nums $n-1 $n $n+1)
      (match (list $n-1 $n $n+1)))))

; === syntax-match? ===

(check
  (false?
    (syntax-match? #'1)))

(check-equal?
  (syntax-match? #'1
    (1 (match "one")))
  (match "one"))

(check
  (false?
    (syntax-match? #'2
      (1 (match "one")))))

(check-equal?
  (syntax-match? #'2
    (1 (match "one"))
    (2 (match "two")))
  (match "two"))

(check-equal?
  (syntax-match? #'foo
    (foo (match "foo")))
  (match "foo"))

(check-equal?
  (syntax-match? #'(foo bar)
    ((foo bar) (match "ok")))
  (match "ok"))

(check
  (false?
    (syntax-match? #'(foo foo)
      ((foo bar) (match "ok")))))

(check-equal?
  (syntax-match? #'(foo bar bar)
    ((foo _ _) (match "ok"))
    (_ #f))
  (match "ok"))

(check-equal?
  (syntax-match? #'(+ 1 2)
    (#'x (match (syntax->datum x))))
  (match '(+ 1 2)))

(check-equal?
  (syntax-match? #'(+ 1 2)
    ((#'x #'y #'z)
      (match (list (syntax->datum x) (syntax->datum y) (syntax->datum z)))))
  (match '(+ 1 2)))

(check-equal?
  (syntax-match? #'"foo"
    ((str s) (match (string-append s "!"))))
  (match "foo!"))

; === syntax-match ===

(check-equal?
  (syntax-match #'"foo"
    ((str s) (match (string-append s "!"))))
  (match "foo!"))

(check
  (raises
    (syntax-match #'123
      ((str s) (match (string-append s "!"))))))

; === syntax-match-ref ===

(check-equal?
  (syntax-match-ref #'"foo"
    ((str s) (match (string-append s "!"))))
  "foo!")

(check
  (raises
    (syntax-match-ref #'123
      ((str s) (match (string-append s "!"))))))

; === syntax-match-ref? ===

(check-equal?
  (syntax-match-ref? #'"foo"
    ((str s) (match (string-append s "!"))))
  "foo!")

(check
  (false?
    (syntax-match-ref? #'123
      ((str s) (match (string-append s "!"))))))

; === syntax-ref-match-ref ===

(check-equal?
  (syntax-ref-match-ref #'"foo"
    ((str s) (string-append s "!")))
  "foo!")

(check
  (raises
    (syntax-ref-match-ref #'123
      ((str s) (string-append s "!")))))

; === syntax-ref-match-ref? ===

(check-equal?
  (syntax-ref-match-ref? #'"foo"
    ((str s) (string-append s "!")))
  "foo!")

(check
  (false?
    (syntax-ref-match-ref? #'123
      ((str s) (string-append s "!")))))
