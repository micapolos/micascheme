(import (micascheme) (leo sentence))

; === quote

(check (equal? (quote-string "'" "foo") "'foo"))

(check (equal? (quote-string? "quote" "'" "foo") "'foo"))
(check (equal? (quote-string? "quote" "'" #f) "quote"))

(check
  (equal?
    (quote-phrase? "quote" "'" '("foo" "123"))
    '("'foo" "123")))

(check
  (equal?
    (quote-phrase? "quote" "'" '(#f "123"))
    '("quote" "123")))

(check
  (equal?
    (quote-sentence? "quote" "'" "foo")
    "'foo"))

(check
  (equal?
    (quote-sentence? "quote" "'" '("foo" "123"))
    '("'foo" "123")))

(check
  (equal?
    (quote-sentence? "quote" "'" '(#f "123"))
    '("quote" "123")))

; === unquote

(check (equal? (unquote-string "`" "foo") "foo`"))

(check (equal? (unquote-string? "unquote" "`" "foo") "foo`"))
(check (equal? (unquote-string? "unquote" "`" #f) "unquote"))

(check
  (equal?
    (unquote-phrase? "unquote" "`" '("foo" "123"))
    '("foo`" "123")))

(check
  (equal?
    (unquote-phrase? "unquote" "`" '(#f "123"))
    '("unquote" "123")))

(check
  (equal?
    (unquote-sentence? "unquote" "`" "foo")
    "foo`"))

(check
  (equal?
    (unquote-sentence? "unquote" "`" '("foo" "123"))
    '("foo`" "123")))

(check
  (equal?
    (unquote-sentence? "unquote" "`" '(#f "123"))
    '("unquote" "123")))

; === quotify / quote

(check
  (equal?
    (sentence-quotify "quote")
    "quote"))

(check
  (equal?
    (sentence-quotify "quasiquote")
    "quasiquote"))

(check
  (equal?
    (sentence-quotify '("quote" . "bar"))
    '("quote" . "bar")))

(check
  (equal?
    (sentence-quotify '("quasiquote" . "bar"))
    '("quasiquote" . "bar")))

(check
  (equal?
    (sentence-quotify '("quote" "foo"))
    '("quote" "foo")))

(check
  (equal?
    (sentence-quotify '("quasiquote" "foo"))
    "'foo'"))

(check
  (equal?
    (sentence-quotify '("quote" "foo" "bar"))
    '("quote" "foo" "bar")))

(check
  (equal?
    (sentence-quotify '("quasiquote" "foo" "bar"))
    '("quasiquote" "foo" "bar")))

(check
  (equal?
    (sentence-quotify '("quote" ("foo" "bar")))
    '("quote" ("foo" "bar"))))

(check
  (equal?
    (sentence-quotify '("quasiquote" ("foo" "bar")))
    '("'foo" "bar'")))

(check
  (equal?
    (sentence-quotify '("quote" ("foo" ("bar" "gar"))))
    '("quote" ("foo" ("bar" "gar")))))

(check
  (equal?
    (sentence-quotify '("quasiquote" ("foo" ("bar" "gar"))))
    '("'foo" ("bar" "gar'"))))

(check
  (equal?
    (sentence-quotify '("quote" ("quote" "a")))
    '("quote" ("quote" "a"))))

(check
  (equal?
    (sentence-quotify '("quasiquote" ("quasiquote" "a")))
    "''a''"))

; === quotify / unquote

(check
  (equal?
    (sentence-quotify "unquote")
    "unquote"))

(check
  (equal?
    (sentence-quotify "unquote-splicing")
    "unquote-splicing"))

(check
  (equal?
    (sentence-quotify '("bar" ("unquote" "foo")))
    '("bar" ("unquote" "foo"))))

(check
  (equal?
    (sentence-quotify '("quasiquote" ("bar" ("unquote" "foo"))))
    '("'bar'" "foo")))

(check
  (equal?
    (sentence-quotify '("bar" ("unquote-splicing" "foo")))
    '("bar" ("unquote-splicing" "foo"))))

(check
  (equal?
    (sentence-quotify '("quasiquote" ("bar" ("unquote-splicing" "foo"))))
    '("'bar'..." "foo")))

(check
  (equal?
    (sentence-quotify '("bar" ("unquote" . "foo")))
    '("bar" ("unquote" . "foo"))))

(check
  (equal?
    (sentence-quotify '("bar" ("unquote-splicing" . "foo")))
    '("bar" ("unquote-splicing" . "foo"))))

; === ->sentence

(check
  (equal?
    (->sentence '())
    '("written" "null")))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence '())
      "null")))

(check
  (equal?
    (->sentence (void))
    '("written" "void")))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence (void))
      "void")))

(check
  (equal?
    (->sentence #t)
    '("written" "true")))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence #t)
      "true")))

(check
  (equal?
    (->sentence #f)
    '("written" "false")))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence #f)
      "false")))

(check
  (equal?
    (->sentence 3.14)
    "3.14"))

(check
  (equal?
    (->sentence #\a)
    '("written" ("char" "a"))))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence #\a)
      '("char" "a"))))

(check
  (equal?
    (->sentence #\:)
    '("written" ("char" "colon"))))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence #\:)
      '("char" "colon"))))

(check
  (equal?
    (parameterize ((quotify-for-display? #t))
      (->sentence #\a))
    "a"))

(check
  (equal?
    (->sentence "foo")
    "\"foo\""))

(check
  (equal?
    (parameterize ((quotify-for-display? #t))
      (->sentence "foo"))
    "foo"))

(check
  (equal?
    (->sentence 'foo)
    "foo"))

(check
  (equal?
    (->sentence '(foo))
    '("foo")))

(check
  (equal?
    (->sentence '(foo ()))
    '("foo" ("written" "null"))))

(check
  (equal?
    (->sentence '(foo bar))
    '("foo" "bar")))

(check
  (equal?
    (->sentence '(foo (bar)))
    '("foo" ("bar"))))

(check
  (equal?
    (->sentence '(123))
    '(#f "123")))

(check
  (equal?
    (->sentence '(123 ()))
    '(#f "123" ("written" "null"))))

(check
  (equal?
    (->sentence '(123 bar))
    '(#f "123" "bar")))

(check
  (equal?
    (->sentence '(123 (bar)))
    '(#f "123" ("bar"))))

(check
  (equal?
    (->sentence '((quote a)))
    '(#f ("quote" "a"))))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence '(123))
      '("list" "123")))

  (check
    (equal?
      (->sentence '(123 ()))
      '("list" "123" "null")))

  (check
    (equal?
      (->sentence '(123 bar))
      '("list" "123" "bar")))

  (check
    (equal?
      (->sentence '(123 (bar)))
      '("list" "123" ("bar"))))

  (check
    (equal?
      (->sentence '((quote a)))
      '("list" ("quote" "a")))))

(check
  (equal?
    (->sentence (box 10))
    '("written" ("box" "10"))))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence (box 10))
      '("box" "10"))))

(check
  (equal?
    (->sentence eof)
    '("written" "eof")))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence eof)
      "eof")))

(check
  (equal?
    (->sentence (bytevector))
    '("written" ("bytevector"))))

(check
  (equal?
    (->sentence (bytevector 1 2 3))
    '("written" ("bytevector" "1" "2" "3"))))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence (bytevector))
      '("bytevector")))

  (check
    (equal?
      (->sentence (bytevector 1 2 3))
      '("bytevector" "1" "2" "3"))))

(check
  (equal?
    (->sentence (vector))
    '("written" ("vector"))))

(check
  (equal?
    (->sentence (vector #\a #\space "foo"))
    '("written"
      ("vector"
        ("written" ("char" "a"))
        ("written" ("char" "space") )
        "\"foo\""))))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence (vector))
      '("vector")))

  (check
    (equal?
      (->sentence (vector #\a #\space "foo"))
      '("vector"
        ("char" "a")
        ("char" "space")
        "\"foo\""))))

(data marker)

(check
  (equal?
    (->sentence marker)
    '("written" ("record" "marker"))))

(data (point x y))

(check
  (equal?
    (->sentence (point 10 20))
    '("written" ("record" ("point" "10" "20")))))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence (point 10 20))
      '("point" "10" "20"))))

(check
  (equal?
    (->sentence +)
    '("written" ("procedure" "+"))))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence +)
      '("procedure" "+"))))

(check
  (equal?
    (->sentence (lambda (x) x))
    '("written" "procedure")))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence (lambda (x) x))
      '"procedure")))

(check
  (equal?
    (->sentence #'+)
    '("written" ("syntax" "+"))))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (->sentence #'+)
      '("syntax" "+"))))

(check
  (equal?
    (list->sentences 123)
    "123"))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (list->sentences 123)
      "123")))

(check
  (equal?
    (list->sentences '(1 2 3))
    '("1" "2" "3")))

(check
  (equal?
    (list->sentences '(1 2 . 3))
    '("1" "2" . "3")))

(parameterize ((pretty-write? #t))
  (check
    (equal?
      (list->sentences '(1 2 . 3))
      '("1" "2" . "3"))))

(let ()
  (define-ftype point (struct (x unsigned-8) (y unsigned-8)))
  (define point-bytevector (bytevector 10 20))
  (define point-ftype-pointer
    (make-ftype-pointer point
      (object->reference-address point-bytevector)))
  (check
    (equal?
      (->sentence point-ftype-pointer)
      '("written" ("ftype" ("point" ("struct" ("x" "10") ("y" "20")))))))

  (parameterize ((pretty-write? #t))
    (check
      (equal?
        (->sentence point-ftype-pointer)
        '("point" ("struct" ("x" "10") ("y" "20")))))))
