(import (check) (syntax) (procedure))

(define-aux-keyword foo)
(check (raises? (lambda () foo)))

(check (syntax-null? #'()))
(check (not (syntax-null? #'(1))))

(check
  (equal?
    (syntax-inline (datum->syntax #'+ 'string?))
    string?))
