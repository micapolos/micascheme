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

(check (equal? (syntax-inline #'string?) string?))
(check (equal? (syntax-inline ((lambda () #'string?))) string?))
(check (equal? (syntax-inline (datum->syntax #'+ 'string?)) string?))
(check (equal? (syntax-inline (predicate-syntax 'string)) string?))
