(import (micascheme) (typed type))

(check (equal? any-boolean any-boolean))
(check (equal? any-fixnum any-fixnum))
(check (equal? any-flonum any-flonum))
(check (equal? any-char any-char))
(check (equal? any-string any-string))
(check (equal? any-type any-type))

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
    (type-apply
      (any-lambda () any-fixnum)
      (list))
    any-fixnum))

; invalid-arg-count
(check
  (raises
    (type-apply
      (any-lambda (any-string) any-fixnum)
      (list any-string any-boolean))))

; invalid-arg
(check
  (raises
    (type-apply
      (any-lambda (any-fixnum any-boolean) any-fixnum)
      (list any-fixnum any-string))))

(check
  (equal?
    (any-list any-string)
    (any-list any-string)))

(check
  (not
    (equal?
      (any-list any-string)
      (any-list any-fixnum))))
