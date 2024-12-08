(import (scheme) (lets) (check) (syntax) (switch) (generate) (syntax-match))

(define-aux-keywords foo bar)

; === pattern-match ===

(check-equal? (pattern-match #'(_ 1 2) _ "ok") "ok")

(check-equal? (pattern-match #'(+ 1 2) #'x (syntax->datum x)) '(+ 1 2))

(check-equal? (pattern-match #'() () "ok") "ok")
(check-equal? (pattern-match #'128 128 "ok") "ok")
(check-equal? (pattern-match #'"foo" "foo" "ok") "ok")
(check-equal? (pattern-match #'#\a #\a "ok") "ok")
(check-equal? (pattern-match #'#t #t "ok") "ok")
(check-equal? (pattern-match #'#f #f "ok") "ok")

(check (raises (pattern-match #'() 123 "ok")))
(check (raises (pattern-match #'123 () "ok")))

(check-equal? (pattern-match #'foo foo "ok") "ok")
(check (raises (pattern-match #'bar foo "ok")))

(check-equal? (pattern-match #'(foo bar) (foo bar) "ok") "ok")
(check (raises (pattern-match #'(foo foo) (foo bar) "ok")))
(check (raises (pattern-match #'(bar bar) (foo bar) "ok")))

(check-equal? (pattern-match? #'() () "ok") "ok")
(check-equal? (pattern-match? #'() 123 "ok") #f)

(let ()
  (define-pattern-matcher str
    (syntax-rules ()
      ((_ (_ s) body)
        (lambda ($syntax)
          (switch-opt (syntax->datum $syntax)
            ((string? $string)
              (lambda ()
                (lets
                  (s $string)
                  body))))))))

  (check-equal? (pattern-match #'"foo" (str s) (string-append s "!")) "foo!")
  (check (raises (pattern-match #'123 (str s) (string-append s "!")))))

(let ()
  (define-pattern-matcher nums
    (syntax-rules ()
      ((_ (_ n-1 n n+1) body)
        (lambda ($syntax)
          (switch-opt (syntax->datum $syntax)
            ((number? $number)
              (lambda ()
                (lets
                  (n-1 (- $number 1))
                  (n $number)
                  (n+1 (+ $number 1))
                  body))))))))

  (check-equal? (pattern-match #'10 (nums $n-1 $n $n+1) (list $n-1 $n $n+1)) (list 9 10 11))
  (check (raises (pattern-match #'"foo" (nums $n-1 $n $n+1) (list $n-1 $n $n+1)))))

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
  (syntax-match #'(foo bar bar)
    ((foo _ _) "ok")
    (_ #f))
  "ok")

(check-equal?
  (syntax-match #'(+ 1 2)
    (#'x (syntax->datum x)))
  '(+ 1 2))

(check-equal?
  (syntax-match #'(+ 1 2)
    ((#'x #'y #'z) (list (syntax->datum x) (syntax->datum y) (syntax->datum z))))
  '(+ 1 2))

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
