(import
  (mica reader)
  (only (micascheme) quote lines-string)
  (leo mica reader line))

(check-reader line
  (ok "123\n" 123)
  (ok "\"foo\"\n" "foo")
  (ok "foo\n" 'foo)
  (ok "foo bar\n" '(foo bar))
  (ok "foo bar goo\n" '(foo (bar goo)))

  ; (ok ":\n" '())
  ; (ok ": 10\n" '(10))
  ; (ok ": 10, 20\n" '(10 20)))

  ; (ok ":\n" '())
  ; (ok ":\n  10\n" '(10))
  ; (ok ":\n  10\n  20\n" '(10 20))

  ; (ok "foo :\n" '(foo ()))
  ; (ok "foo bar :\n" '(foo (bar ()))))

  (ok "foo, " 'foo)
  (error "123 "))

(check-reader lines
  (ok "" '())
  (ok "10\n" '(10))
  (ok "10\n20\n" '(10 20))
  (ok "foo\nfoo bar\n" '(foo (foo bar))))

(check-reader line
  (error "foo:")
  (error "foo: ")
  (ok "foo:\n" 'foo)
  (ok "foo: 10\n" '(foo 10))
  (ok "foo: 10, 20\n" '(foo 10 20))
  (ok "foo:\n  10\n" '(foo 10))
  (ok "foo:\n  10\n  20\n" '(foo 10 20)))

(check-reader line
  (ok
    (lines-string
      "point"
      "  x 10"
      "  y 20")
    '(point (x 10) (y 20)))
  (ok
    (lines-string
      "circle"
      "  center point"
      "    x 10"
      "    y 10"
      "  radius 10")
    '(circle
      (center
        (point
          (x 10)
          (y 10)))
      (radius 10)))
  ; (ok
  ;   (lines-string
  ;     "point"
  ;     ""
  ;     "  x 10"
  ;     ""
  ;     ""
  ;     "  y 20"
  ;     ""
  ;     "")
  ;   '(point (x 10) (y 20)))
  )
