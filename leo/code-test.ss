(import (scheme) (check) (limited) (leo code))

(check
  (equal?
    (limited-length+leo?
      (make-limited? 10 2)
      '())
    (make-limited? 10 2)))

(check
  (equal?
    (limited-length+leo?
      (make-limited? 10 2)
      "foo")
    (make-limited? 11 1)))

(check
  (equal?
    (limited-length+leo?
      (make-limited? 10 2)
      '(foo bar))
    (make-limited? 12 0)))

(check
  (not
    (limited-length+leo?
      (make-limited? 10 2)
      '(foo (bar goo)))))

(check
  (equal?
    (limited-length+leo?
      (make-limited? 10 5)
      (bytevector 1 2 3))
    (make-limited? 14 1)))

(check
  (not
    (limited-length+leo?
      (make-limited? 10 5)
      (bytevector 1 2 3 4 5))))

(check
  (equal?
    (limited-length+leo?
      (make-limited? 10 5)
      (vector "foo" '(foo bar)))
    (make-limited? 14 1)))

(check
  (not
    (limited-length+leo?
      (make-limited? 10 5)
      (vector "foo" '(foo bar) '(zoo zar)))))

