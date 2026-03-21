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
