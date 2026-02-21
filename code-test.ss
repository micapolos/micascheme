(import (scheme) (check) (code))

; empty-code
(check-code=? (empty-code) "")

; char-code
(check-code=? (char-code #\newline) "\n")
(check-code=? (char-code #\a) "a")

; code-indent
(check-code=? (code-indent #f) "")
(check-code=? (code-indent (char-code #\newline)) "\n")
(check-code=? (code-indent (char-code #\a)) "  a")

; string-code
(check-code=? (string-code "") "")
(check-code=? (string-code "foo\nbar\n") "foo\nbar\n")

; number-code
(check-code=? (number-code 10) "10")
(check-code=? (number-code 3.14) "3.14")

; code-append
(check-code=?
  (apply code-append
    (list
      (string-code "foo")
      (char-code #\newline)
      (string-code "bar")
      (char-code #\newline)))
  "foo\nbar\n")

; list->code
(check-code=?
  (list->code
    (list
      (string-code "foo")
      (char-code #\newline)
      (string-code "bar")
      (char-code #\newline)))
  "foo\nbar\n")

; code syntax
(check-code=?
  (code
    (code)
    (code #\f "oo")
    #\b
    "ar"
    12
    (char-code #\3)
    (string-code (string-append "4" "5"))
    (code 6 7 8))
  "foobar12345678")

(check-code=? (separated-code ":") "")
(check-code=? (separated-code ":" (code "a")) "a")
(check-code=? (separated-code ":" (code "a") (code "b") (code "c")) "a:b:c")

(check-code=? (separated-code ":" (code "a") #f (code "c")) "a:c")
(check-code=? (separated-code ":" (code "a") (empty-code) (code "c")) "a:c")
(check-code=? (separated-code ":" (code "a") (string-code "") (code "c")) "a:c")

(check-code=? (space-separated-code (code "a") (code "b") (code "c")) "a b c")
(check-code=? (newline-separated-code (code "a") (code "b") (code "c")) "a\nb\nc")
(check-code=? (emptyline-separated-code (code "a") (code "b") (code "c")) "a\n\nb\n\nc")

(check-code=? (suffixed-code ":") "")
(check-code=? (suffixed-code ":" (code "a")) "a:")
(check-code=? (suffixed-code ":" (code "a") (code "b") (code "c")) "a:b:c:")

(check-code=? (colon-ended-code (code "a") (code "b") (code "c")) "a;b;c;")
(check-code=? (newline-ended-code (code "a") (code "b") (code "c")) "a\nb\nc\n")
(check-code=? (indented-code "\na" "b" "\nc") "\n  ab\n  c")

(check-code=? (code-in-round-brackets (code "a") (code "b") (code "c")) "(abc)")
(check-code=? (code-in-square-brackets (code "a") (code "b") (code "c")) "[abc]")
(check-code=? (code-in-angle-brackets (code "a") (code "b") (code "c")) "<abc>")
(check-code=? (code-in-curly-brackets (code "a") (code "b") (code "c")) "{abc}")
(check-code=? (code-in-newlines (code "a") (code "b") (code "c")) "\nabc\n")

(check-code=? (list->separated-code (code " ") (list (code "a") (code "b") (code "c"))) "a b c")
