(import (scheme) (check) (lim) (leo code))

(check
  (equal?
    (lim+line-length?
      (make-lim? 10 2)
      '())
    (make-lim? 10 2)))

(check
  (equal?
    (lim+line-length?
      (make-lim? 10 2)
      "foo")
    (make-lim? 11 1)))

(check
  (equal?
    (lim+line-length?
      (make-lim? 10 2)
      '(foo bar))
    (make-lim? 12 0)))

(check
  (not
    (lim+line-length?
      (make-lim? 10 2)
      '(foo (bar goo)))))

(check
  (equal?
    (lim+line-length?
      (make-lim? 10 5)
      (bytevector 1 2 3))
    (make-lim? 14 1)))

(check
  (not
    (lim+line-length?
      (make-lim? 10 5)
      (bytevector 1 2 3 4 5))))

(check
  (equal?
    (lim+line-length?
      (make-lim? 10 5)
      (vector "foo" '(foo bar)))
    (make-lim? 14 1)))

(check
  (not
    (lim+line-length?
      (make-lim? 10 5)
      (vector "foo" '(foo bar) '(zoo zar)))))

