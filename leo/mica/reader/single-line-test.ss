(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica reader)
  (leo mica reader single-line))

(check-reader (map single-line-annotation %annotation-stripped)
  ; empty list
  (ok "()" '())

  ; singleton list
  (ok "(foo)" '(foo))
  (ok "(123)" '(123))
  (ok "(\"foo\")" '("foo"))

  ; proper lists
  (ok "(foo, 2, 3)" '(foo 2 3))
  (ok "(1, 2, 3)" '(1 2 3))
  (ok "(\"foo\", 2, 3)" '("foo" 2 3))

  ; literal
  (ok "123" 123)
  (ok "\"foo\"" "foo")
  (ok "foo" 'foo)

  (error "1 2")
  (error "\"foo\" 2")

  ; sentences
  (ok "foo ()" '(foo))
  (ok "foo bar" '(foo bar))
  (ok "foo (bar)" '(foo bar))
  (ok "foo 1" '(foo 1))
  (ok "foo (1)" '(foo 1))
  (ok "foo bar gar" '(foo (bar gar)))
  (ok "foo (bar, gar)" '(foo bar gar))
  (ok "foo (2, 3)" '(foo 2 3))
  (ok "foo ()" '(foo))
  (ok "foo bar ()" '(foo (bar)))
  (ok "foo (bar, gar)" '(foo bar gar))

  ; begin-quote
  (ok "'foo" ''foo)
  (ok "`foo" '`foo)
  (ok "'`'foo" ''`'foo)

  ; end-quote
  (ok "foo` bar" '(foo ,bar))
  (ok "foo`... bar" '(foo ,@bar))
  (ok "foo`... (bar, gar)" '(foo (unquote-splicing bar gar)))
  (ok "foo``... bar" '(foo ,,@bar))

  (ok
    "circle (radius 10, center point (x 20, y 30))"
    '(circle (radius 10) (center (point (x 20) (y 30))))))

(check-reader (map single-line-annotation/eof %annotation-stripped)
  (ok "123" 123)
  (ok "\n123" 123)
  (ok "123\n" 123)
  (ok "foo" 'foo)
  (ok "foo\n" 'foo)
  (ok "foo ()" '(foo))
  (ok "foo ()\n" '(foo))

  (ok
    "circle (radius 10, center point (x 20, y 30))"
    '(circle (radius 10) (center (point (x 20) (y 30))))))
