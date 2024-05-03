(import (scheme) (check) (identifier) (symbol))

(check (equal? (identifier-named? #`foo foo) #t))
(check (equal? (identifier-named? #`foo bar) #f))

(check
  (equal?
    (syntax->datum
      (build-identifier ($string #'dupa) (string-append $string "-jasiu")))
    'dupa-jasiu))

(check
  (free-identifier=?
    (identifier-append #'+ #'string #'- #'append)
    #'string-append))
