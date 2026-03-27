(import (micascheme) (leo sentence))

; === quote

(check (equal? (quote-string "'" "foo") "'foo"))

(check (equal? (quote-string? "'" "foo") "'foo"))
(check (equal? (quote-string? "'" #f) #f))

(check (equal? (quote-phrase? "'" '(#f 123)) #f))

(check
  (equal?
    (quote-phrase? "'" '("foo" 123))
    '("'foo" 123)))

(check
  (equal?
    (quote-sentence? "'" "foo")
    "'foo"))

(check (equal? (quote-sentence? "'" '(#f 123)) #f))

(check
  (equal?
    (quote-sentence? "'" '("foo" 123))
    '("'foo" 123)))

; === unquote

(check (equal? (unquote-string "`" "foo") "foo`"))

(check (equal? (unquote-string? "`" "foo") "foo`"))
(check (equal? (unquote-string? "`" #f) #f))

(check (equal? (unquote-phrase? "`" '(#f 123)) #f))

(check
  (equal?
    (unquote-phrase? "`" '("foo" 123))
    '("foo`" 123)))

(check
  (equal?
    (unquote-sentence? "`" "foo")
    "foo`"))

(check (equal? (unquote-sentence? "`" '(#f 123)) #f))

(check
  (equal?
    (unquote-sentence? "`" '("foo" 123))
    '("foo`" 123)))

; === quotify / quote

(check
  (equal?
    (sentence-quotify "quote")
    "quote"))

(check
  (equal?
    (sentence-quotify '("quote" . bar))
    '("quote" . bar)))

(check
  (equal?
    (sentence-quotify '("quote" foo))
    "'foo"))

(check
  (equal?
    (sentence-quotify '("quote" foo bar))
    '("quote" foo bar)))

(check
  (equal?
    (sentence-quotify '("quote" (foo bar)))
    '("'foo" bar)))

(check
  (equal?
    (sentence-quotify '("quote" (foo (bar gar))))
    '("'foo" (bar gar))))

; === quotify / unquote

(check
  (equal?
    (sentence-quotify "unquote")
    "unquote"))

(check
  (equal?
    (sentence-quotify '("bar" (unquote . foo)))
    '("bar`" . foo)))

(check
  (equal?
    (sentence-quotify '("bar" (unquote foo)))
    '("bar`" foo)))

; === ->sentence

(check
  (equal?
    (->sentence '())
    "null"))

(check
  (equal?
    (->sentence #t)
    "true"))

(check
  (equal?
    (->sentence #f)
    "false"))

(check
  (equal?
    (->sentence #f)
    "false"))

(check
  (equal?
    (->sentence 3.14)
    "3.14"))

(check
  (equal?
    (->sentence #\a)
    '("char" a)))

(check
  (equal?
    (->sentence #\:)
    '("char" colon)))

(check
  (equal?
    (->sentence "foo")
    "\"foo\""))

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
    '("foo" ())))

(check
  (equal?
    (->sentence '(foo bar))
    '("foo" bar)))

(check
  (equal?
    (->sentence '(foo (bar)))
    '("foo" (bar))))

(check
  (equal?
    (->sentence '(123))
    '("list" 123)))

(check
  (equal?
    (->sentence '(123 ()))
    '("list" 123 ())))

(check
  (equal?
    (->sentence '(123 bar))
    '("list" 123 bar)))

(check
  (equal?
    (->sentence '(123 (bar)))
    '("list" 123 (bar))))

(check
  (equal?
    (->sentence (box 10))
    '("box" 10)))

(check
  (equal?
    (->sentence (bytevector))
    '("bytevector")))

(check
  (equal?
    (->sentence (bytevector 1 2 3))
    '("bytevector" 1 2 3)))

(check
  (equal?
    (->sentence (vector))
    '("vector")))

(check
  (equal?
    (->sentence (vector #\a #\space "foo"))
    '("vector" #\a #\space "foo")))

(data (point x y))

(check
  (equal?
    (->sentence (point 10 20))
    '("point" 10 20)))

(check
  (equal?
    (->sentence +)
    '("procedure" +)))

(check
  (equal?
    (->sentence (lambda (x) x))
    "procedure"))

(check
  (equal?
    (->sentence #'+)
    '("syntax" +)))

(check
  (equal?
    (list->sentences 123)
    "123"))

(check
  (equal?
    (list->sentences '(1 2 3))
    '("1" "2" "3")))

(check
  (equal?
    (list->sentences '(1 2 . 3))
    '("1" "2" . "3")))
