(import (micascheme) (parser) (leo parser))

(check (obj=? (parse (line-parser) "") (parse-error 1 1)))

(check (obj=? (parse (line-parser) "128") 128))
(check (obj=? (parse (line-parser) "-128") -128))
(check (obj=? (parse (line-parser) "+128") 128))
(check (obj=? (parse (line-parser) "\"foo\"") "foo"))
(check (obj=? (parse (line-parser) "foo") `foo))

(check (obj=? (parse (line-parser) "foo bar") `(foo bar)))
(check (obj=? (parse (line-parser) "foo bar zoo") `(foo (bar zoo))))
(check (obj=? (parse (line-parser) "foo 128") `(foo 128)))
(check (obj=? (parse (line-parser) "foo \"foo\"") `(foo "foo")))

(check (obj=? (parse (line-parser) "foo\n  bar") `(foo bar)))
(check (obj=? (parse (line-parser) "foo\n  bar\n    zoo") `(foo (bar zoo))))

(check (obj=? (parse (line-parser) "foo\n  128\n  \"foo\"") `(foo 128 "foo")))

(check (obj=? (parse (line-parser) "foo: 128") `(foo 128)))
(check (obj=? (parse (line-parser) "foo: 128, bar") `(foo 128 bar)))
(check (obj=? (parse (line-parser) "foo: 128, bar, \"goo\"") `(foo 128 bar "goo")))
(check (obj=? (parse (line-parser) "foo: 128, bar goo, zar") `(foo 128 (bar goo) zar)))

(check (obj=? (parse (line-parser) "point()") `point))
(check (obj=? (parse (line-parser) "point(x)") `(point x)))
(check (obj=? (parse (line-parser) "point(x, y)") `(point x y)))
(check (obj=? (parse (line-parser) "point(x 10, y 20)") `(point (x 10) (y 20))))
(check (obj=? (parse (line-parser) "point(x: 10, y: 20)") `(point (x 10) (y 20))))
(check (obj=? (parse (line-parser) "point(x: 10, 20)") `(point (x 10) 20)))
(check (obj=? (parse (line-parser) "point(x: 10, y: 20, 30)") `(point (x 10) (y 20) 30)))

(check (obj=? (parse (script-parser) "") `()))
(check (obj=? (parse (script-parser) "\n") `()))
(check (obj=? (parse (script-parser) "foo") (parse-error 1 4)))
(check (obj=? (parse (script-parser) "foo\n") `(foo)))
(check (obj=? (parse (script-parser) "foo\n128\n") `(foo 128)))
(check (obj=? (parse (script-parser) "foo\n128\n\"foo\"\n") `(foo 128 "foo")))

(check (obj=? (parse (script-parser) "foo bar\ngoo gar\n") `((foo bar) (goo gar))))
(check (obj=? (parse (script-parser) "foo\n  bar\ngoo\n  gar\n") `((foo bar) (goo gar))))

(check (obj=? (parse (script-parser) "text Hello, world!\nthe end\n") `((text "Hello, world!") (the end))))

(check
  (obj=?
    (parse (script-parser) "\n\npoint\n  x: 10\n\n  y: 20\nplus point\n\n\n  x: 30\n  y: 40\n\n\n")
    `((point (x 10) (y 20)) (plus (point (x 30) (y 40))))))
