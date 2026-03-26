(import (micascheme) (leo sentence))

; === primitive-string

(check (equal? (primitive-string "foo") "#foo"))

(parameterize ((primitive-string-pretty? #t))
  (check (equal? (primitive-string "foo") "foo")))

; === quote

(check (equal? (quote-string "'" "foo") "'foo"))

(check (equal? (quote-string? "'" "foo") "'foo"))
(check (equal? (quote-string? "'" #f) #f))

(check (equal? (quote-phrase? "'" (phrase #f 123)) #f))

(check
  (equal?
    (quote-phrase? "'" (phrase "foo" 123))
    (phrase "'foo" 123)))

(check
  (equal?
    (quote-sentence? "'" "foo")
    "'foo"))

(check (equal? (quote-sentence? "'" (phrase #f 123)) #f))

(check
  (equal?
    (quote-sentence? "'" (phrase "foo" 123))
    (phrase "'foo" 123)))

; === unquote

(check (equal? (unquote-string "`" "foo") "foo`"))

(check (equal? (unquote-string? "`" "foo") "foo`"))
(check (equal? (unquote-string? "`" #f) #f))

(check (equal? (unquote-phrase? "`" (phrase #f 123)) #f))

(check
  (equal?
    (unquote-phrase? "`" (phrase "foo" 123))
    (phrase "foo`" 123)))

(check
  (equal?
    (unquote-sentence? "`" "foo")
    "foo`"))

(check (equal? (unquote-sentence? "`" (phrase #f 123)) #f))

(check
  (equal?
    (unquote-sentence? "`" (phrase "foo" 123))
    (phrase "foo`" 123)))

; === quotify

(check (equal? (sentence-quotify "quote") "quote"))
(check (equal? (sentence-quotify (phrase "quote" 'bar)) "'bar"))
(check (equal? (sentence-quotify "quasiquote") "quasiquote"))
(check (equal? (sentence-quotify (phrase "quasiquote" 'bar)) "`bar"))

; (check (equal? (sentence-quotify "unquote") "unquote"))
; (check (equal? (sentence-quotify (phrase "bar" (phrase "unquote" 'foo))) (phrase "bar`" 'foo)))
; (check (equal? (sentence-quotify "quasiquote") "quasiquote"))
; (check (equal? (sentence-quotify (phrase "quasiquote" 'bar)) "`bar"))

; === ->sentence

(check
  (equal?
    (->sentence '())
    "#null"))

(check
  (equal?
    (->sentence #t)
    "#true"))

(check
  (equal?
    (->sentence #f)
    "#false"))

(check
  (equal?
    (->sentence #f)
    "#false"))

(check
  (equal?
    (->sentence 3.14)
    "3.14"))

(check
  (equal?
    (->sentence #\a)
    (phrase "#char" '(a))))

(check
  (equal?
    (->sentence #\:)
    (phrase "#char" '(colon))))

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
    (phrase "foo" '())))

(check
  (equal?
    (->sentence '(foo ()))
    (phrase "foo" '(()))))

(check
  (equal?
    (->sentence '(foo bar))
    (phrase "foo" '(bar))))

(check
  (equal?
    (->sentence '(foo (bar)))
    (phrase "foo" '((bar)))))

(check
  (equal?
    (->sentence '(123))
    (phrase #f '(123))))

(check
  (equal?
    (->sentence '(123 ()))
    (phrase #f '(123 ()))))

(check
  (equal?
    (->sentence '(123 bar))
    (phrase #f '(123 bar))))

(check
  (equal?
    (->sentence '(123 (bar)))
    (phrase #f '(123 (bar)))))

(check
  (equal?
    (->sentence (box 10))
    (phrase "#box" '(10))))

(check
  (equal?
    (->sentence (bytevector))
    (phrase "#bytevector" '())))

(check
  (equal?
    (->sentence (bytevector 1 2 3))
    (phrase "#bytevector" '(1 2 3))))

(check
  (equal?
    (->sentence (vector))
    (phrase "#vector" '())))

(check
  (equal?
    (->sentence (vector #\a #\space "foo"))
    (phrase "#vector" '(#\a #\space "foo"))))

(data (point x y))

(check
  (equal?
    (->sentence (point 10 20))
    (phrase "#point" '(10 20))))

(check
  (equal?
    (->sentence +)
    (phrase "#procedure" '(+))))

(check
  (equal?
    (->sentence (lambda (x) x))
    "#procedure"))

(check
  (equal?
    (->sentence #'+)
    (phrase "#syntax" '(+))))

