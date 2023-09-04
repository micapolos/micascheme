(import 
  (micascheme) 
  (only (parser) parse parse-error) 
  (leo parser))

(check (obj=? (parse (script-parser) "") `()))
(check (obj=? (parse (script-parser) "128") `(128)))
(check (obj=? (parse (script-parser) "128\n\"foo\"") `(128 "foo")))
(check (obj=? (parse (script-parser) "128\n\"foo\"\nfoo") `(128 "foo" foo)))

(check (obj=? (parse (line-parser) "") (parse-error 1 1)))

(check (obj=? (parse (line-parser) "128") 128))
(check (obj=? (parse (line-parser) "\"foo\"") "foo"))
(check (obj=? (parse (line-parser) "foo") `foo))

(check (obj=? (parse (line-parser) "foo bar") `(foo bar)))
(check (obj=? (parse (line-parser) "foo bar zoo") `(foo (bar zoo))))
(check (obj=? (parse (line-parser) "foo 128") `(foo 128)))
(check (obj=? (parse (line-parser) "foo \"foo\"") `(foo "foo")))

(check (obj=? (parse (line-parser) "foo\n  bar") `(foo bar)))
(check (obj=? (parse (line-parser) "foo\n  bar\n    zoo") `(foo (bar zoo))))

(check (obj=? (parse (line-parser) "foo\n  128\n  \"foo\"") `(foo 128 "foo")))
