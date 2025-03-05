(import (micascheme) (typed type))

(check (equal? any-boolean any-boolean))
(check (equal? any-fixnum any-fixnum))
(check (equal? any-flonum any-flonum))
(check (equal? any-char any-char))
(check (equal? any-string any-string))

(check (not (equal? any-string any-fixnum)))

(check
  (equal?
    (any-lambda () any-fixnum)
    (any-lambda () any-fixnum)))
(check
  (equal?
    (any-lambda (any-string any-fixnum) any-fixnum)
    (any-lambda (any-string any-fixnum) any-fixnum)))

(check
  (not
    (equal?
      (any-lambda () any-fixnum)
      (any-lambda () any-string))))

(check
  (not
    (equal?
      (any-lambda (any-fixnum any-string) any-fixnum)
      (any-lambda (any-string any-fixnum) any-fixnum))))

(check
  (equal?
    (type-apply (any-lambda () any-fixnum))
    any-fixnum))

; invalid-arg-count
(check
  (equal?
    (type-apply
      (any-lambda (any-string) any-fixnum)
      any-string any-boolean)
    any-fixnum))

; invalid-arg
(check
  (raises
    (type-apply
      (any-lambda (any-fixnum any-boolean) any-fixnum)
      any-fixnum any-string)))
