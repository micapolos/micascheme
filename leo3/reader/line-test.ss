(import
  (mica reader)
  (only (micascheme) quote lines-string)
  (leo3 reader line))

(check-reader atom
  ; literals
  (ok "123" 123)
  (ok "\"foo\"" "foo")

  ; identifiers
  (ok "foo" 'foo)
  (ok "foo bar" 'foo-bar)
  (ok "foo bar goo" 'foo-bar-goo))

(check-reader (inline?-non-empty-lines #t)
  ; empty
  (error "")

  ; single literal
  (ok "10\n" '(10))
  (ok "\"foo\"\n" '("foo"))

  ; single identifier
  (ok "foo\n" '(foo))
  (ok "foo bar\n" '(foo-bar))
  (ok "foo as bar\n" '(foo->bar))
  (ok "is foo bar\n" '(foo-bar?))

  ; colon space
  (error "foo: \n")
  (ok "foo: 10\n" '((foo 10)))
  (ok "foo: 10, 20\n" '((foo 10 20)))

  ; colon newline
  (ok "foo:\n" '((foo)))
  (ok "foo:\n  10\n" '((foo 10)))
  (ok "foo:\n  10\n  20\n" '((foo 10 20)))

  ; comma-separated
  (ok "10, 20\n" '(10 20))
  (ok "10, foo bar, \"bar\"\n" '(10 foo-bar "bar")))

(check-reader (inline?-lines #t)
  (ok "" '())
  (ok "10\n" '(10)))

(check-reader (inline?-non-empty-lines #f)
  ; empty
  (error "")

  ; single literal
  (ok "10\n" '(10))
  (ok "\"foo\"\n" '("foo"))

  ; single identifier
  (ok "foo\n" '(foo))
  (ok "foo bar\n" '(foo-bar))
  (ok "foo as bar\n" '(foo->bar))
  (ok "is foo bar\n" '(foo-bar?))

  ; many lines
  (ok "10\n20\n" '(10 20))
  (ok "10\nfoo\nfoo bar\n" '(10 foo foo-bar))

  ; colon space
  (error "foo: \n")
  (ok "foo: 10\n" '((foo 10)))
  (ok "foo: 10, 20\n" '((foo 10 20)))
  (ok "foo: 10, 20\nbar\n" '((foo 10 20) bar))
  )

(check-reader (inline?-lines #f)
  (ok "" '())
  (ok "10\n" '(10)))
