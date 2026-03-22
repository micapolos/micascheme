(import (scheme) (check) (limited) (boolean) (code) (leo code))

; === line-code

(check-code=? (line-code '()) "#null")

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

(check-code=? (line-code (box 123)) "#box 123")
(check-code=? (line-code (box '(foo bar))) "#box foo bar")
(check-code=? (line-code (box '(foo . bar))) "#box foo . bar")
(check-code=? (line-code (box '(foo bar gar))) "#box foo (bar, gar)")

(check-code=? (line-code (bytevector)) "#bytevector ()")
(check-code=? (line-code (bytevector 1 2 3)) "#bytevector (1, 2, 3)")

(check-code=? (line-code (vector)) "#vector ()")
(check-code=?
  (line-code (vector '() #t 123 #\a "foo" 'foo '(foo bar) '(foo . bar)))
  "#vector (#null, #true, 123, #char a, \"foo\", foo, foo bar, foo . bar)")

(check-code=?
  (line-code '(circle (radius 10) (center (point (x 10) (y 20)))))
  "circle (radius 10, center point (x 10, y 20))")

; === limited-simple-line-code?

(check-simple-line-code 1 '() "#null")
(check-simple-line-code 1 #t "#true")
(check-simple-line-code 1 123 "123")
(check-simple-line-code 1 #\a "#char a")
(check-simple-line-code 1 "foo" "\"foo\"")
(check-simple-line-code 1 'foo "foo")

(check-simple-line-code 2 (box 1) "#box 1")
(check-simple-line-code 3 (box '(foo bar)) "#box foo bar")

(check-simple-line-code 2 (bytevector 1) "#bytevector 1")
(check-simple-line-code-false? 1 (bytevector))
(check-simple-line-code-false? 3 (bytevector 1 2))

(check-simple-line-code 2 (vector 1) "#vector 1")
(check-simple-line-code 3 (vector '(foo bar)) "#vector foo bar")
(check-simple-line-code-false? 3 (vector))
(check-simple-line-code-false? 3 (vector 1 2))

(check-simple-line-code 2 '(foo bar) "foo bar")
(check-simple-line-code 2 '(foo . bar) "foo . bar")
(check-simple-line-code 3 '(foo (bar goo)) "foo bar goo")
(check-simple-line-code 3 '(foo (bar goo)) "foo bar goo")
(check-simple-line-code-false? 3 '(foo))
(check-simple-line-code-false? 3 '(foo bar goo))

