(import (scheme) (check) (syntax) (procedure) (boolean))

(check (equal? (identifiers? #'()) #t))
(check (equal? (identifiers? #'(foo bar)) #t))
(check (equal? (identifiers? #'(foo 123)) #f))

(define-aux-keyword foo)
(check (raises? (lambda () foo)))

(check (syntax-null? #'()))
(check (not (syntax-null? #'(1))))

(define (predicate-syntax s)
  (datum->syntax #'+
    (string->symbol
      (string-append
        (symbol->string s)
        "?"))))

(check (free-identifier=? (syntax-selector #'+) #'+))
(check (free-identifier=? (syntax-selector #'(+ a b)) #'+))
(check (false? (syntax-selector #'"foo")))
(check (false? (syntax-selector #'("foo"))))

(check (free-identifier=? (syntax-pattern-id #'+) #'+))
(check (free-identifier=? (syntax-pattern-id #'(+ a b)) #'+))

(check (free-identifier=? (syntax-rule-id #'(+ body)) #'+))
(check (free-identifier=? (syntax-rule-id #'((+ a b) body)) #'+))

(check (false? (syntax-case-opt #'foo ())))

(check
  (equal?
    (expand
      `(inline-indexed ($i 4) $i)
      (environment '(scheme) '(syntax)))
    `(begin 0 1 2 3)))

; --- syntax=?

(check (syntax=? #`10 #`10))
(check (not (syntax=? #`10 #`20)))

(check (syntax=? #`foo #`foo))
(check (not (syntax=? #`foo #`bar)))

(check (syntax=? #`"foo" #`"foo"))
(check (not (syntax=? #`"foo" #`"bar")))

(check (syntax=? #`() #`()))
(check (syntax=? #`(+) #`(+)))
(check (syntax=? #`(+ -) #`(+ -)))
(check (syntax=? #`(+ - *) #`(+ - *)))
(check (syntax=? #`(+ - . *) #`(+ - . *)))

(check (not (syntax=? #`() #`(+))))
(check (not (syntax=? #`(+) #`(-))))
(check (not (syntax=? #`(+) #`(+ -))))
(check (not (syntax=? #`(+ -) #`(+))))
(check (not (syntax=? #`(+ . -) #`(+ . *))))

; --- replace-identifiers

(check
  (syntax=?
    (syntax-replace #'+ #'- #'(+ 10 (+ 20 (* 30 40))))
    #'(- 10 (- 20 (* 30 40)))))
