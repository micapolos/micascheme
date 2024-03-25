(import (check) (syntax) (procedure))

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

(define-namespace matcher)
(define-matcher x "matcher-x")
(define-matcher y "matcher-y")
(check (equal? (matcher x) "matcher-x"))
(check (equal? (matcher y) "matcher-y"))
;(check (equal? (matcher z) #f)) => syntax error

(check (free-identifier=? (syntax-pattern-id? #'+) #'+))
(check (free-identifier=? (syntax-pattern-id? #'(+ a b)) #'+))
(check (false? (syntax-pattern-id? #'(10 a b))))

(check (free-identifier=? (syntax-rule-id? #'(+ body)) #'+))
(check (free-identifier=? (syntax-rule-id? #'((+ a b) body)) #'+))
(check (false? (syntax-rule-id? #'((10 a b) fender body))))
