(import
  (micascheme)
  (leo writer))

(check (equal? (datums-string) ""))

(check (equal? (datums-string foo) "foo\n"))
(check (equal? (datums-string 10) "10\n"))
(check (equal? (datums-string "foo") "\"foo\"\n"))

(check (equal? (datums-string foo bar) "foo\nbar\n"))

(check (equal? (datums-string (foo bar)) "foo bar\n"))
(check (equal? (datums-string (foo (bar zoo))) "foo bar zoo\n"))
(check (equal? (datums-string (foo bar zoo)) "foo\n  bar\n  zoo\n"))

(check (equal? (datums-string (point (x 10) (y 20))) "point\n  x 10\n  y 20\n"))
