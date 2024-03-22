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

(check
  (equal?
    (syntax->datum
      (syntax-map-identifiers #`(+ (+ "foo" "bar") "zoo" other)
        (lambda ($identifier)
          (cond
            ((bound-identifier=? $identifier #'+) #'string-append)
            (else $identifier)))))
    `(string-append (string-append "foo" "bar") "zoo" other)))
