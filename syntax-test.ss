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
    (syntax-inline #`(string-append "foo" "bar"))
    "foobar"))
