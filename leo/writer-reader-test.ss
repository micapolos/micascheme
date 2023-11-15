(import
	(micascheme)
	(leo writer-reader))

(check (equal? (script-string) ""))

;(check (equal? (script-string foo) "foo\n"))
(check (equal? (script-string 10) "10\n"))
(check (equal? (script-string "foo") "\"foo\"\n"))

;(check (equal? (script-string foo bar) "foo\nbar\n"))
;(check (equal? (script-string (foo)) "foo\n"))
;(check (equal? (script-string (foo bar)) "foo bar\n"))
