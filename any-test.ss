(import (scheme) (check) (any))

(check (any-boolean? any-boolean))
(check (any-symbol? any-symbol))
(check (any-char? any-char))
(check (any-string? any-string))
(check (any-fixnum? any-fixnum))
(check (any-flonum? any-flonum))
(check (any-integer? any-integer))
(check (any-type? any-type))
(check (any-lambda? (any-lambda (any-string any-boolean) any-flonum)))

(check (equal? any-boolean any-boolean))
(check (equal? any-char any-char))
(check (equal? any-string any-string))
(check (equal? any-fixnum any-fixnum))
(check (equal? any-flonum any-flonum))
(check (equal? any-integer any-integer))
(check (equal? any-type any-type))

(check
  (equal?
    (any-lambda (any-string any-char) any-string)
    (any-lambda (any-string any-char) any-string)))

(check
  (not
    (equal?
      (any-lambda (any-string any-char) any-string)
      (any-lambda (any-string any-char) any-char))))

(check
  (not
    (equal?
      (any-lambda (any-string) any-string)
      (any-lambda (any-string any-char) any-string))))

(check
  (not
    (equal?
      (any-lambda (any-string any-string) any-string)
      (any-lambda (any-string any-char) any-string))))
