(import
	(micascheme)
	(leo writing-reader))

(check (equal? (script-string) ""))

(check (equal? (script-string 10) "10\n"))
(check (equal? (script-string "foo") "\"foo\"\n"))

(check (equal? (script-string foo) "foo\n"))
(check (equal? (script-string foo bar) "foo\nbar\n"))
(check (equal? (script-string (foo)) "foo\n"))

(check (equal? (script-string (foo bar)) "foo bar\n"))
(check (equal? (script-string (foo (bar goo))) "foo bar goo\n"))
(check (equal? (script-string (foo bar goo)) "foo\n  bar\n  goo\n"))
