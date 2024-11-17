(import (scheme) (check) (code))

; empty-code
(check (equal? (code-string (empty-code)) ""))

; char-code
(check (equal? (code-string (char-code #\newline)) "\n"))
(check (equal? (code-string (char-code #\a)) "a"))

; code-indent
(check (equal? (code-string (code-indent (char-code #\newline))) "\n"))
(check (equal? (code-string (code-indent (char-code #\a))) "  a"))

; string-code
(check (equal? (code-string (string-code "")) ""))
(check (equal? (code-string (string-code "foo\nbar\n")) "foo\nbar\n"))

; number-code
(check (equal? (code-string (number-code 10)) "10"))
(check (equal? (code-string (number-code 3.14)) "3.14"))

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

(check (equal? (code-string (separated-code ":")) ""))
(check (equal? (code-string (separated-code ":" (code "a"))) "a"))
(check (equal? (code-string (separated-code ":" (code "a") (code "b") (code "c"))) "a:b:c"))

(check (equal? (code-string (separated-code ":" (code "a") #f (code "c"))) "a:c"))
(check (equal? (code-string (separated-code ":" (code "a") (empty-code) (code "c"))) "a:c"))
(check (equal? (code-string (separated-code ":" (code "a") (string-code "") (code "c"))) "a:c"))

(check (equal? (code-string (space-separated-code (code "a") (code "b") (code "c"))) "a b c"))
(check (equal? (code-string (newline-separated-code (code "a") (code "b") (code "c"))) "a\nb\nc"))
(check (equal? (code-string (emptyline-separated-code (code "a") (code "b") (code "c"))) "a\n\nb\n\nc"))

(check (equal? (code-string (suffixed-code ":")) ""))
(check (equal? (code-string (suffixed-code ":" (code "a"))) "a:"))
(check (equal? (code-string (suffixed-code ":" (code "a") (code "b") (code "c"))) "a:b:c:"))

(check (equal? (code-string (colon-ended-code (code "a") (code "b") (code "c"))) "a;b;c;"))
(check (equal? (code-string (newline-ended-code (code "a") (code "b") (code "c"))) "a\nb\nc\n"))

(check (equal? (code-string (code-in-round-brackets (code "a") (code "b") (code "c"))) "(abc)"))
(check (equal? (code-string (code-in-square-brackets (code "a") (code "b") (code "c"))) "[abc]"))
(check (equal? (code-string (code-in-angle-brackets (code "a") (code "b") (code "c"))) "<abc>"))
(check (equal? (code-string (code-in-curly-brackets (code "a") (code "b") (code "c"))) "{abc}"))
