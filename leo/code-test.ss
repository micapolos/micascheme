(import
  (scheme)
  (check)
  (limited)
  (boolean)
  (code)
  (leo code))

; === atom-code?

(check-atom-code? '() #f)

(check-atom-code? #t #f)
(check-atom-code? #f #f)

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

(check-atom-code? (bytevector) #f)
(check-atom-code? (bytevector 1) #f)

(check-atom-code? (vector) #f)
(check-atom-code? (vector 1) #f)

; === line-code

(check-code=? (line-code '()) "written null")

(check-code=? (line-code #t) "written true")
(check-code=? (line-code #f) "written false")

(check-code=? (line-code 123) "123")
(check-code=? (line-code 3.14) "3.14")
(check-code=? (line-code -123) "-123")

(check-code=? (line-code #\a) "written char a")
(check-code=? (line-code #\0) "written char 0")
(check-code=? (line-code #\space) "written char space")
(check-code=? (line-code #\.) "written char dot")
(check-code=? (line-code #\newline) "written char newline")

(check-code=? (line-code "") "\"\"")
(check-code=? (line-code "foo") "\"foo\"")

(check-code=? (line-code '(foo . bar)) "foo bar.")
(check-code=? (line-code '(foo gar . bar)) "foo (gar, bar.)")
(check-code=? (line-code '(foo)) "foo ()")
(check-code=? (line-code '(foo bar)) "foo bar")
(check-code=? (line-code '((foo bar))) ": foo bar")
(check-code=? (line-code '(foo (bar goo))) "foo bar goo")
(check-code=? (line-code '(foo bar goo)) "foo (bar, goo)")
(check-code=? (line-code '(foo bar (goo gar))) "foo (bar, goo gar)")
(check-code=? (line-code '(foo (bar zar) (goo gar))) "foo (bar zar, goo gar)")
(check-code=? (line-code '(foo (bar zar) (goo gar mar))) "foo (bar zar, goo (gar, mar))")

(check-code=? (line-code (box 123)) "written box 123")
(check-code=? (line-code (box '(foo bar))) "written box foo bar")
(check-code=? (line-code (box '(foo . bar))) "written box foo bar.")
(check-code=? (line-code (box '(foo bar gar))) "written box foo (bar, gar)")

(check-code=? (line-code (bytevector)) "written bytevector ()")
(check-code=? (line-code (bytevector 1 2 3)) "written bytevector (1, 2, 3)")

(check-code=? (line-code (vector)) "written vector ()")
(check-code=?
  (line-code (vector '() #t 123 #\a "foo" 'foo '(foo bar) '(foo . bar)))
  "written vector (written null, written true, 123, written char a, \"foo\", foo, foo bar, foo bar.)")

(check-code=?
  (line-code '(circle (radius 10) (center (point (x 10) (y 20)))))
  "circle (radius 10, center point (x 10, y 20))")

; === space-line-code?-limiter

(check-space-line-code 2 '() "written null")
(check-space-line-code 2 #t "written true")
(check-space-line-code 1 123 "123")
(check-space-line-code 3 #\a "written char a")
(check-space-line-code 1.5 "foo" "\"foo\"")
(check-space-line-code 1 'foo "foo")

(check-space-line-code 2 '(foo bar) "foo bar")
(check-space-line-code-false? 2 '((foo bar)))
(check-space-line-code 2 '(foo . bar) "foo bar.")
(check-space-line-code-false? 3 '(foo bar . gar))
(check-space-line-code 3 '(foo (bar goo)) "foo bar goo")
(check-space-line-code 3 '(foo (bar . goo)) "foo bar goo.")
(check-space-line-code-false? 3 '(foo))
(check-space-line-code-false? 3 '(foo bar goo))

(check-space-line-code 3 (box 1) "written box 1")
(check-space-line-code 4 (box '(foo bar)) "written box foo bar")

(check-space-line-code 3 (bytevector 1) "written bytevector 1")
(check-space-line-code-false? 1 (bytevector))
(check-space-line-code-false? 3 (bytevector 1 2))

(check-space-line-code 3 (vector 1) "written vector 1")
(check-space-line-code 4 (vector '(foo bar)) "written vector foo bar")
(check-space-line-code-false? 3 (vector))
(check-space-line-code-false? 3 (vector 1 2))

; === colon-line-code?-limiter

(check-colon-line-code 2 (bytevector) "written bytevector:")
(check-colon-line-code 3 (bytevector 1) "written bytevector 1")
(check-colon-line-code 4 (bytevector 1 2) "written bytevector: 1, 2")
(check-colon-line-code 5 (bytevector 1 2 3) "written bytevector: 1, 2, 3")

(check-colon-line-code 2 (vector) "written vector:")
(check-colon-line-code 4 (vector '(foo bar)) "written vector foo bar")
(check-colon-line-code 6 (vector '(foo bar) '(goo gar)) "written vector: foo bar, goo gar")
(check-colon-line-code 8 (vector '(foo bar) '(goo gar) '(zoo zar)) "written vector: foo bar, goo gar, zoo zar")

(check-colon-line-code 5 (vector '(foo bar gar)) "written vector foo: bar, gar")

(check-colon-line-code 2 '(foo bar) "foo bar")
(check-colon-line-code 2 '(foo . bar) "foo bar.")
(check-colon-line-code 3 '(foo bar gar) "foo: bar, gar")
(check-colon-line-code 3 '(foo bar . gar) "foo: bar, gar.")
(check-colon-line-code 4 '(foo (bar . gar) zar) "foo: bar gar., zar")
(check-colon-line-code 4 '(foo (bar . gar) . zar) "foo: bar gar., zar.")
(check-colon-line-code 5 '(foo (bar . gar) (zar . tar)) "foo: bar gar., zar tar.")

(check-colon-line-code 2 '() "written null")
(check-colon-line-code 2 '(1) ": 1")
(check-colon-line-code 3 '(1 2) ":: 1, 2")  ; WTF?

(check-colon-line-code-false? 5 '(foo (bar gar . zar) var))
(check-colon-line-code-false? 4 '((foo bar) (goo gar)))
(check-colon-line-code-false? 2 '(1 bar))
(check-colon-line-code-false? 3 '(1 foo bar))
(check-colon-line-code-false? 4 '(foo bar ((gar zar))))

(check-colon-line-code 5 '(point (x 10) (y 20)) "point: x 10, y 20")
(check-colon-line-code 6 '(center (point (x 10) (y 20))) "center point: x 10, y 20")

; === block-code

(check-block-code '() "written null")
(check-block-code #t "written true")
(check-block-code 123 "123")
(check-block-code #\a "written char a")
(check-block-code "foo" "\"foo\"")
(check-block-code 'foo "foo")
(check-block-code '(foo bar) "foo bar")
(check-block-code '(foo . bar) "foo bar.")
(check-block-code '(foo bar gar) "foo: bar, gar")
(check-block-code '(foo bar . gar) "foo: bar, gar.")
(check-block-code '(foo bar ((gar zar)))
  "foo: bar, : gar zar")
(check-block-code '(1 2 3) ":: 1, 2, 3")

(parameterize ((code-line-limit 5))
  ; TODO: We want items to be comma separated, 5 in each line
  (check-block-code '(1 2 3 4 5 6 7 8 9 10)
    ":" "  1" "  2" "  3" "  4" "  5" "  6" "  7" "  8" "  9" "  10")

  ; TODO: We want to wrap v6-v10 in an intented newline
  (check-block-code '(v1 (v2 (v3 (v4 (v5 (v6 (v7 (v8 (v9 v10)))))))))
     "v1 v2 v3 v4 v5 v6 v7 v8 v9 v10"))

(parameterize ((code-line-limit 3))
  (check-block-code '(foo 1 2)
    "foo: 1, 2")
  (check-block-code '(foo 1 2 3)
    "foo"
    "  1"
    "  2"
    "  3")
  (check-block-code '(foo 1 . 2)
    "foo: 1, 2.")
  (check-block-code '(foo 1 2 . 3)
    "foo"
    "  1"
    "  2"
    "  3."))

(parameterize ((code-line-limit 6))
  (check-block-code '(foo (a1 a2) (b1 b2))
    "foo: a1 a2, b1 b2")
  (check-block-code '(foo (a1 a2) (b1 b2) (c1 c2))
    "foo"
    "  a1 a2"
    "  b1 b2"
    "  c1 c2")
  (check-block-code '(foo (a1 . a2) (b1 . b2))
    "foo: a1 a2., b1 b2.")
  (check-block-code '(foo (a1 . a2) (b1 . b2) (c1 . c2))
    "foo"
    "  a1 a2."
    "  b1 b2."
    "  c1 c2.")
  (check-block-code '(foo (a1 a2) (b1 b2) . c1)
    "foo: a1 a2, b1 b2, c1.")
  (check-block-code '(foo (a1 . a2) (b1 . b2) c1 . c2)
    "foo"
    "  a1 a2."
    "  b1 b2."
    "  c1"
    "  c2."))

(check-block-code '((x 10 20) (y 30 40)) ":" "  x: 10, 20" "  y: 30, 40")
(check-block-code '(foo (x 10 20) (y 30 40)) "foo" "  x: 10, 20" "  y: 30, 40")

(check-block-code (bytevector) "written bytevector:")
(check-block-code (bytevector 1 2 3) "written bytevector: 1, 2, 3")
(check-block-code
  (bytevector 1 2 3 4 5 6 7 8 9 10)
  "written bytevector" "  1" "  2" "  3" "  4" "  5" "  6" "  7" "  8" "  9" "  10")

(check-block-code (vector) "written vector:")
(check-block-code (vector 1 2 3) "written vector: 1, 2, 3")
(check-block-code
  (vector '(foo bar goo) '(zoo zar zoo))
  "written vector"
  "  foo: bar, goo"
  "  zoo: zar, zoo")

(check-block-code
  '(circle (radius 10) (center (point (x 10) (y 20))))
  "circle"
  "  radius 10"
  "  center point: x 10, y 20")
