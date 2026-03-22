(import (scheme) (check) (limited) (boolean) (code) (leo code))

; === line-code

(check-code=? (line-code '()) "()")

(check-code=? (line-code #t) "#true")
(check-code=? (line-code #f) "#false")

(check-code=? (line-code 123) "123")
(check-code=? (line-code 3.14) "3.14")
(check-code=? (line-code -123) "-123")

(check-code=? (line-code #\a) "#char a")
(check-code=? (line-code #\0) "#char 0")
(check-code=? (line-code #\space) "#char space")
(check-code=? (line-code #\.) "#char .")
(check-code=? (line-code #\newline) "#char newline")

(check-code=? (line-code "") "\"\"")
(check-code=? (line-code "foo") "\"foo\"")

(check-code=? (line-code '(foo . bar)) "foo . bar")
(check-code=? (line-code '(foo gar . bar)) "foo (gar . bar)")
(check-code=? (line-code '(foo)) "(foo)")
(check-code=? (line-code '(foo bar)) "foo bar")
(check-code=? (line-code '(foo (bar goo))) "foo bar goo")
(check-code=? (line-code '(foo bar goo)) "foo (bar, goo)")
(check-code=? (line-code '(foo bar (goo gar))) "foo (bar, goo gar)")
(check-code=? (line-code '(foo (bar zar) (goo gar))) "foo (bar zar, goo gar)")
(check-code=? (line-code '(foo (bar zar) (goo gar mar))) "foo (bar zar, goo (gar, mar))")

(check-code=?
  (line-code '(circle (radius 10) (center (point (x 10) (y 20)))))
  "circle (radius 10, center point (x 10, y 20))")

; === line-code

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

(check-code=? (atom-code? 'null) "null")
(check-code=? (atom-code? 'false) "false")
(check-code=? (atom-code? 'true) "true")
(check-code=? (atom-code? 123) "123")
(check-code=? (atom-code? 'foo) "foo")
(check-code=? (atom-code? "foo") "\"foo\"")
(check (false? (atom-code? '(foo))))
(check (false? (atom-code? '(char a))))
(check (false? (atom-code? '(foo . bar))))
(check (false? (atom-code? '(bytevector 1 2 3))))
(check (false? (atom-code? '(vector 1 2 3))))

; === limited-simple-code?

(check
  (limited=? string=?
    (limited-simple-string? 'foo 10)
    (make-limited? "foo" 9)))

; (check
;   (limited=? string=?
;     (limited-simple-string? '(foo) 10)
;     (make-limited? "foo" 9)))

; (check
;   (limited=? string=?
;     (limited-simple-string? '(foo bar) 10)
;     (make-limited? "foo bar" 8)))

; (check
;   (limited=? string=?
;     (limited-simple-string? '(foo . bar) 10)
;     (make-limited? "foo . bar" 8)))

; (check
;   (limited=? string=?
;     (limited-simple-string? '(foo (bar goo)) 10)
;     (make-limited? "foo bar goo" 7)))
