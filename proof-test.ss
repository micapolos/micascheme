(import (scheme) (check) (proof))

(check (equal? (syntax->proof #'#f) boolean?))
(check (equal? (syntax->proof #'128) number?))
(check (equal? (syntax->proof #'"foo") string?))

(check
  (raises?
    (lambda ()
      (syntax->proof #'string-length))))

(check
  (equal?
    (syntax->proof
      (lambda ($identifier)
        (and (free-identifier=? $identifier #'string-length)
          (lambda? (string?) number?)))
      #'string-length)
    (lambda? (string?) number?)))

