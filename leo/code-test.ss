(import (scheme) (check) (limited) (boolean) (code) (leo code))

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

; === atom-code?

(check-code=? (atom-code? '()) "null")
(check-code=? (atom-code? #f) "false")
(check-code=? (atom-code? #t) "true")
(check-code=? (atom-code? 123) "123")
(check-code=? (atom-code? 'foo) "foo")
(check-code=? (atom-code? "foo") "\"foo\"")
(check (false? (atom-code? #\a)))
(check (false? (atom-code? '(foo . bar))))
(check (false? (atom-code? (bytevector))))
(check (false? (atom-code? (vector))))

; === limited-simple-code?

(check
  (limited=? string=?
    (limited-simple-string? #t 10)
    (make-limited? "true" 9)))

(check
  (limited=? string=?
    (limited-simple-string? #\a 10)
    (make-limited? "char a" 8)))

(check
  (limited=? string=?
    (limited-simple-string? #\space 10)
    (make-limited? "char space" 8)))

(check
  (limited=? string=?
    (limited-simple-string? '(foo bar) 10)
    (make-limited? "foo bar" 8)))

(check
  (limited=? string=?
    (limited-simple-string? '(foo . bar) 10)
    (make-limited? "foo . bar" 8)))

; (check
;   (limited=? string=?
;     (limited-simple-string? '(foo (bar goo)) 10)
;     (make-limited? "foo bar goo" 7)))
