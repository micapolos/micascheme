(import (scheme) (check) (limited) (boolean) (code) (leo code))

; === atom-code?

(check-atom-code? '() "#null")

(check-atom-code? #t "#true")
(check-atom-code? #f "#false")

(check-atom-code? 123 "123")
(check-atom-code? 3.14 "3.14")
(check-atom-code? -123 "-123")

(check-atom-code? #\a #f)

(check-atom-code? "" "\"\"")
(check-atom-code? "foo" "\"foo\"")

(check-atom-code? 'foo "foo")
(check-atom-code? '(foo bar) #f)
(check-atom-code? '(foo . bar) #f)

(check-atom-code? (box 123) #f)

(check-atom-code? (bytevector) "#bytevector")
(check-atom-code? (bytevector 1) #f)

(check-atom-code? (vector) "#vector")
(check-atom-code? (vector 1) #f)

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

; === space-line-code?-limiter

(check-space-line-code 1 '() "#null")
(check-space-line-code 1 #t "#true")
(check-space-line-code 1 123 "123")
(check-space-line-code 1 #\a "#char a")
(check-space-line-code 1 "foo" "\"foo\"")
(check-space-line-code 1 'foo "foo")

(check-space-line-code 2 '(foo bar) "foo bar")
(check-space-line-code 2 '(foo . bar) "foo . bar")
(check-space-line-code 3 '(foo (bar goo)) "foo bar goo")
(check-space-line-code 3 '(foo (bar goo)) "foo bar goo")
(check-space-line-code-false? 3 '(foo))
(check-space-line-code-false? 3 '(foo bar goo))

(check-space-line-code 2 (box 1) "#box 1")
(check-space-line-code 3 (box '(foo bar)) "#box foo bar")

(check-space-line-code 2 (bytevector 1) "#bytevector 1")
(check-space-line-code-false? 1 (bytevector))
(check-space-line-code-false? 3 (bytevector 1 2))

(check-space-line-code 2 (vector 1) "#vector 1")
(check-space-line-code 3 (vector '(foo bar)) "#vector foo bar")
(check-space-line-code-false? 3 (vector))
(check-space-line-code-false? 3 (vector 1 2))

; === colon-line-code?-limiter

(check-colon-line-code 1 (bytevector) "#bytevector:")
(check-colon-line-code 2 (bytevector 1) "#bytevector 1")
(check-colon-line-code 3 (bytevector 1 2) "#bytevector: 1, 2")
(check-colon-line-code 4 (bytevector 1 2 3) "#bytevector: 1, 2, 3")

(check-colon-line-code 1 (vector) "#vector:")
(check-colon-line-code 3 (vector '(foo bar)) "#vector foo bar")
(check-colon-line-code 5 (vector '(foo bar) '(goo gar)) "#vector: foo bar, goo gar")
(check-colon-line-code 7 (vector '(foo bar) '(goo gar) '(zoo zar)) "#vector: foo bar, goo gar, zoo zar")

(check-colon-line-code-false? 4 (vector '(foo bar gar)))

(check-colon-line-code 2 '(foo bar) "foo bar")
(check-colon-line-code 3 '(foo bar gar) "foo: bar, gar")
(check-colon-line-code 4 '((foo bar) (goo gar)) ": foo bar, goo gar")

; === block-code

(check-block-code '() "#null")
(check-block-code #t "#true")
(check-block-code 123 "123")
(check-block-code #\a "#char a")
(check-block-code "foo" "\"foo\"")
(check-block-code 'foo "foo")
(check-block-code '(foo bar) "foo bar")
(check-block-code '(foo bar gar) "foo: bar, gar")

(check-block-code '((x 10 20) (y 30 40)) ":" "  x: 10, 20" "  y: 30, 40")
(check-block-code '(foo (x 10 20) (y 30 40)) "foo" "  x: 10, 20" "  y: 30, 40")

(check-block-code (bytevector) "#bytevector:")
(check-block-code (bytevector 1 2 3) "#bytevector: 1, 2, 3")
(check-block-code
  (bytevector 1 2 3 4 5 6 7 8 9 10)
  "#bytevector" "  1" "  2" "  3" "  4" "  5" "  6" "  7" "  8" "  9" "  10")

(check-block-code (vector) "#vector:")
(check-block-code (vector 1 2 3) "#vector: 1, 2, 3")
; (check-block-code
;   (vector '(foo bar goo) '(zoo zar zoo))
;   "#vector"
;   "  foo: bar, goo"
;   "  zoo: zar, zoo")
