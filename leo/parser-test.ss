(import (micascheme) (parser) (leo parser))

(check (obj=? (parse (script-parser) "") `()))
(check (obj=? (parse (script-parser) "foo") `(foo)))
(check (obj=? (parse (script-parser) "foo\n128") `(foo 128)))
(check (obj=? (parse (script-parser) "foo\n128\n\"foo\"") `(foo 128 "foo")))

(check (obj=? (parse (script-parser) "foo bar\ngoo gar") `((foo bar) (goo gar))))
(check (obj=? (parse (script-parser) "foo\n  bar\ngoo\n  gar") `((foo bar) (goo gar))))

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

(check
  (obj=?
    (parse (script-parser) "point\n  x: 10\n  y: 20\nplus point\n  x: 30\n  y: 40")
    `((point (x 10) (y 20)) (plus (point (x 30) (y 40))))))
