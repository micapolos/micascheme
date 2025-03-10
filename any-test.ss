(import (scheme) (check) (any))

(check (any? (any "foo")))
(check (any-boolean? any-boolean))
(check (any-symbol? any-symbol))
(check (any-char? any-char))
(check (any-string? any-string))
(check (any-fixnum? any-fixnum))
(check (any-flonum? any-flonum))
(check (any-integer? any-integer))
(check (any-type? any-type))
(check (any-list? (any-list any-string)))
(check (any-lambda? (any-lambda (any-string any-boolean) any-flonum)))
(check (any-integer-between? (any-integer-between 0 10)))
(check (any-any-list? any-any-list))
(check (any-any-lambda? any-any-lambda))

(check (equal? (any "foo") (any "foo")))
(check (equal? any-boolean any-boolean))
(check (equal? any-char any-char))
(check (equal? any-string any-string))
(check (equal? any-fixnum any-fixnum))
(check (equal? any-flonum any-flonum))
(check (equal? any-integer any-integer))
(check (equal? any-type any-type))
(check (equal? any-any-list any-any-list))
(check (equal? any-any-lambda any-any-lambda))

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

(check (equal? (any-integer-between 0 10) (any-integer-between 0 10)))
(check (not (equal? (any-integer-between 0 10) (any-integer-between 1 10))))
(check (not (equal? (any-integer-between 0 10) (any-integer-between 0 11))))
(check (not (equal? (any-integer-between 0 10) any-integer)))
