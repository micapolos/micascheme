(import (scheme) (check) (code))

; empty-code
(check (equal? (code-string empty-code) ""))

; char-code
(check (equal? (code-string (char-code #\newline)) "\n"))
(check (equal? (code-string (char-code #\a)) "a"))

; code-indent
(check (equal? (code-string (code-indent (char-code #\newline))) "\n"))
(check (equal? (code-string (code-indent (char-code #\a))) "  a"))

; string-code
(check (equal? (code-string (string-code "foo\nbar\n")) "foo\nbar\n"))

; code-append
(check
  (equal?
    (code-string
      (apply code-append
        (list
          (string-code "foo")
          (char-code #\newline)
          (string-code "bar")
          (char-code #\newline))))
    "foo\nbar\n"))

; code syntax
(check
  (equal?
    (code-string
      (code
        (code)
        (code #\f "oo")
        #\b
        "ar"
        12
        (char-code #\3)
        (string-code (string-append "4" "5"))
        (code 6 7 8)))
    "foobar12345678"))
