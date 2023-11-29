(import (check) (proof) (proven))

(define-proof string-length
  (lambda? (string?) number?))

(check
  (equal?
    (proof string-length)
    (lambda? (string?) number?)))

(check (equal? (proven #f) #f))
(check (equal? (proven 128) 128))
(check (equal? (proven "foo") "foo"))

(check
  (equal?
    (proven string-length)
    string-length))

(check
  (equal?
    (proven (string-length "foo"))
    3))
