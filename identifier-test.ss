(import (check) (identifier))

(check (equal? (identifier-named? #`foo foo) #t))
(check (equal? (identifier-named? #`foo bar) #f))

(check
  (equal?
    (syntax->datum
      (build-identifier ($string #'dupa) (string-append $string "-jasiu")))
    'dupa-jasiu))
