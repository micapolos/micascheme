(import (micascheme) (parser) (leo parser))

(check (equal? (parse (line-parser) "") (parse-error 1 1)))

(check (equal? (parse (line-parser) "128") 128))
(check (equal? (parse (line-parser) "-128") -128))
(check (equal? (parse (line-parser) "+128") 128))
(check (equal? (parse (line-parser) "\"foo\"") "foo"))
(check (equal? (parse (line-parser) "foo") `foo))

(check (equal? (parse (line-parser) "foo bar") `(foo bar)))
(check (equal? (parse (line-parser) "foo bar zoo") `(foo (bar zoo))))
(check (equal? (parse (line-parser) "foo 128") `(foo 128)))
(check (equal? (parse (line-parser) "foo \"foo\"") `(foo "foo")))

(check (equal? (parse (line-parser) "foo\n  bar") `(foo bar)))
(check (equal? (parse (line-parser) "foo\n  bar\n    zoo") `(foo (bar zoo))))

(check (equal? (parse (line-parser) "foo\n  128\n  \"foo\"") `(foo 128 "foo")))

(check (equal? (parse (line-parser) "foo: 128") `(foo 128)))
(check (equal? (parse (line-parser) "foo: 128, bar") `(foo 128 bar)))
(check (equal? (parse (line-parser) "foo: 128, bar, \"goo\"") `(foo 128 bar "goo")))
(check (equal? (parse (line-parser) "foo: 128, bar goo, zar") `(foo 128 (bar goo) zar)))

(check (equal? (parse (line-parser) "point()") `point))
(check (equal? (parse (line-parser) "point(x)") `(point x)))
(check (equal? (parse (line-parser) "point(x, y)") `(point x y)))
(check (equal? (parse (line-parser) "point(x 10, y 20)") `(point (x 10) (y 20))))
(check (equal? (parse (line-parser) "point(x: 10, y: 20)") `(point (x 10) (y 20))))
(check (equal? (parse (line-parser) "point(x: 10, 20)") `(point (x 10) 20)))
(check (equal? (parse (line-parser) "point(x: 10, y: 20, 30)") `(point (x 10) (y 20) 30)))

(check (equal? (parse (script-parser) "") `()))
(check (equal? (parse (script-parser) "\n") `()))
(check (equal? (parse (script-parser) "foo") (parse-error 1 4)))
(check (equal? (parse (script-parser) "foo\n") `(foo)))
(check (equal? (parse (script-parser) "foo\n128\n") `(foo 128)))
(check (equal? (parse (script-parser) "foo\n128\n\"foo\"\n") `(foo 128 "foo")))

(check (equal? (parse (script-parser) "foo bar\ngoo gar\n") `((foo bar) (goo gar))))
(check (equal? (parse (script-parser) "foo\n  bar\ngoo\n  gar\n") `((foo bar) (goo gar))))

(check
  (equal?
    (parse (script-parser) "\n\npoint\n  x: 10\n\n  y: 20\nplus point\n\n\n  x: 30\n  y: 40\n\n\n")
    `((point (x 10) (y 20)) (plus (point (x 30) (y 40))))))
