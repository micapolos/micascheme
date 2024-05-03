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

(run-void
  (with-ellipsis ___
    (define-syntax foo
      (syntax-rules ()
        ((_ $id ___ $last) (list $id ___ $last))))
    (define-syntax bar
      (syntax-rules ()
        ((_ $id ___ $last) (list $id ___ $last)))))
  (check (equal? (foo 1 2 3 4 5) (list 1 2 3 4 5)))
  (check (equal? (bar 1 2 3 4 5) (list 1 2 3 4 5))))

(check
  (equal?
    (expand `(inline-indexed ($i 4) (foobar $i)))
    `(begin (foobar 0) (foobar 1) (foobar 2) (foobar 3))))
