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
  (ok "foo: 10, 20\n" '((foo 10 20))))

(check-reader (inline?-lines #f)
  (ok "" '())
  (ok "10\n" '(10)))

(check-reader line
  ; literals
  (ok "123\n" 123)
  (ok "\"foo\"\n" "foo")

  ; identifiers
  (ok "foo\n" 'foo)
  (ok "foo bar\n" 'foo-bar)
  (ok "foo bar goo\n" 'foo-bar-goo)

  ; comma space
  (ok "foo: 10\n" '(foo 10))
  (ok "foo: \"bar\"\n" '(foo "bar"))
  (ok "foo: bar\n" '(foo bar))
  (ok "foo: bar goo\n" '(foo bar-goo))
  (ok "foo bar: goo\n" '(foo-bar goo))
  (ok "foo bar: goo gar\n" '(foo-bar goo-gar))

  (error ":\n")
  (error ": 10\n")
  (error ": 10, 20\n")
  (error ": 10, 20\n")
  (error ": foo 10\n")
  (error ": foo, 10\n")

;   (ok ":\n" '())
;   (ok ":\n  10\n" '(10))
;   (ok ":\n  10\n  20\n" '(10 20))

;   (ok "foo :\n" '(foo ()))
;   (ok "foo bar :\n" '(foo (bar ())))

;   ;(ok "foo, " 'foo)
;   (error "123 "))
  )

(check-reader lines
  (ok "" '())
  (ok "10\n" '(10))
  (ok "10\n20\n" '(10 20))
  (ok "foo\nfoo bar\n" '(foo foo-bar))
  (ok "\n\nfoo\n\nfoo bar\n\n" '(foo foo-bar))
  (error "\n\nfoo 123\n\nfoo bar\n\n")
  ;(ok ": 10, 20\n: 30, 40\n" '((10 20) (30 40)))
  )

(check-reader line
  (error "foo:")
  (error "foo: ")
  (ok "foo:\n" '(foo))
  (ok "foo:\n  10\n" '(foo 10))
  (ok "foo:\n  10\n  20\n" '(foo 10 20)))

; (check-reader line
;   (ok
;     (lines-string
;       "point"
;       "  x 10"
;       "  y 20")
;     '(point (x 10) (y 20)))

;   (ok
;     (lines-string
;       "circle"
;       "  center point"
;       "    x 10"
;       "    y 10"
;       "  radius 10")
;     '(circle
;       (center
;         (point
;           (x 10)
;           (y 10)))
;       (radius 10)))

;   (ok
;     (lines-string
;       "circle"
;       ""
;       "  center point"
;       ""
;       "    x 10"
;       ""
;       "    y 10"
;       ""
;       "  radius 10"
;       "")
;     '(circle
;       (center
;         (point
;           (x 10)
;           (y 10)))
;       (radius 10))))

; (check-reader lines
;   (ok
;   (lines-string
;     "" ""
;     "define hello \"hello\""
;     "" ""
;     "define world \"world\""
;     ""
;     "define"
;     "  exclamate string"
;     "  string-append: string, \"!\""
;     "")
;   '(
;     (define (hello "hello"))
;     (define (world "world"))
;     (define (exclamate string)
;       (string-append string "!")))))
