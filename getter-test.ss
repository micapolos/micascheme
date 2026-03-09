(import
  (scheme)
  (check)
  (getter)
  (eof)
  (annotation))

(check-gets string-getter "" "" 0 0 0)
(check-gets string-getter "\n" "\n" 1 1 0)
(check-gets string-getter "a" "a" 1 0 1)
(check-gets string-getter "ab" "ab" 2 0 2)
(check-gets string-getter "ab\n" "ab\n" 3 1 0)
(check-gets string-getter "ab\nc" "ab\nc" 4 1 1)
(check-gets string-getter "ab\ncd" "ab\ncd" 5 1 2)
(check-gets string-getter "ab\ncd\n" "ab\ncd\n" 6 2 0)

(check-gets (or-eof-getter string-getter) "" eof 0 0 0)
(check-gets (or-eof-getter string-getter) "abc" "abc" 3 0 3)

(check-gets (skip-char-getter string-getter) "abc" "bc" 3 0 3)

(check-gets (indented-getter string-getter) "" "" 0 0 0)
(check-gets (indented-getter string-getter) "\n" "\n" 1 1 0)
(check-gets (indented-getter string-getter) "\n\n" "\n\n" 2 2 0)
(check-gets (indented-getter string-getter) "a" "" 0 0 0) ; unindented
(check-get-raises (indented-getter string-getter) " ")  ; incomplete indent
(check-get-raises (indented-getter string-getter) " a") ; invalid indent char
(check-get-raises (indented-getter string-getter) "  ") ; empty indent
(check-get-raises (indented-getter string-getter) "  \n") ; indent-only line
(check-gets (indented-getter string-getter) "  a" "a" 3 0 3)
(check-gets (indented-getter string-getter) "  ab" "ab" 4 0 4)
(check-gets (indented-getter string-getter) "  a\n" "a\n" 4 1 0)
(check-gets (indented-getter string-getter) "  a\nb" "a\n" 4 1 0) ; unindented
(check-get-raises (indented-getter string-getter) "  a\n ") ; incomplete indent
(check-get-raises (indented-getter string-getter) "  a\n  ") ; empty indent
(check-gets (indented-getter string-getter) "  a\n  b" "a\nb" 7 1 3)
(check-gets (indented-getter string-getter) "  a\n  b\n" "a\nb\n" 8 2 0)
(check-gets (indented-getter string-getter) "  a\n\n  b\n" "a\n\nb\n" 9 3 0) ; skipping newlines
(check-gets (indented-getter string-getter) "  a\n\n  b\n" "a\n\nb\n" 9 3 0) ; skipping newlines

(check-gets
  (annotation-getter string-getter stripped-annotation)
  "abc"
  (stripped-annotation "abc" (make-source-object test-sfd 0 3))
  3)

(check-gets
  (annotation-getter
    (list-getter (annotation-getter char-getter stripped-annotation))
    list-annotation)
  "abc"
  (list-annotation
    (list
      (stripped-annotation #\a (make-source-object test-sfd 0 1))
      ( stripped-annotation #\b (make-source-object test-sfd 1 2))
      (stripped-annotation #\c (make-source-object test-sfd 2 3)))
    (make-source-object test-sfd 0 3))
  3)

(check-gets eof?-getter "" #t 0)
(check-gets eof?-getter "abc" #f 0)

(check-get-raises char-getter "")
(check-gets char-getter "abc" #\a 1)

(check-gets char/eof-getter "" eof 0)
(check-gets char/eof-getter "abc" #\a 1)

(check-gets (exact-char-getter #\a) "abc" #\a 1)
(check-get-raises (exact-char-getter #\a) "")
(check-get-raises (exact-char-getter #\a) "bca")

(check-gets (exact-getter #\a) "abc" #\a 1)
(check-get-raises (exact-getter #\a) "")
(check-get-raises (exact-getter #\a) "bca")

(check-gets (exact-string-getter "foo") "foo" "foo" 3)
(check-gets (exact-string-getter "foo") "foobar" "foo" 3)
(check-get-raises (exact-string-getter "foo") "")
(check-get-raises (exact-string-getter "foo") "fo")

(check-gets (exact-getter "foo") "foo" "foo" 3)
(check-gets (exact-getter "foo") "foobar" "foo" 3)
(check-get-raises (exact-getter "foo") "")
(check-get-raises (exact-getter "foo") "fo")

(check-gets peek-char/eof-getter "" eof 0)
(check-gets peek-char/eof-getter "abc" #\a 0)

(check-gets
  (append-getter char/eof-getter char/eof-getter)
  "abc"
  '(#\a #\b)
  2)

(check-gets (skip-until-getter char-whitespace? string-getter) "" eof 0)
(check-gets (skip-until-getter char-whitespace? string-getter) "   " eof 3)
(check-gets (skip-until-getter char-whitespace? string-getter) "   foo" "foo" 6)

(check-gets (string-while-getter char-alphabetic?) "" "" 0)
(check-gets (string-while-getter char-alphabetic?) "foo" "foo" 3)
(check-gets (string-while-getter char-alphabetic?) "foo123" "foo" 3)

(check-gets alphabetic-string-getter "foo123" "foo" 3 0 3)
(check-gets numeric-string-getter "123abc" "123" 3 0 3)

(check-gets bfp-getter "" 0 0 0 0)
(check-gets line-number-getter "" 1 0 0 0)
(check-gets column-number-getter "" 1 0 0 0)

(check-gets (append-getter string-getter bfp-getter) "ab\ncd\ne" (list "ab\ncd\ne" 7) 7 2 1)
(check-gets (append-getter string-getter line-number-getter) "ab\ncd\ne" (list "ab\ncd\ne" 3) 7 2 1)
(check-gets (append-getter string-getter column-number-getter) "ab\ncd\ne" (list "ab\ncd\ne" 2) 7 2 1)

(check-gets (starting-getter (exact-getter #\<) char-getter) "<b" #\b 2)
(check-gets (starting-getter (exact-getter #\<) char-getter) "<bc" #\b 2)
(check-get-raises (starting-getter (exact-getter #\<) char-getter) "<")

(check-gets
  (enclosing-getter (exact-getter #\<) char-getter (exact-getter #\>))
  "<b>" #\b 3)

(check-gets
  (enclosing-getter (exact-getter #\<) char-getter (exact-getter #\>))
  "<b>d" #\b 3)

(check-get-raises
  (enclosing-getter (exact-getter #\<) char-getter (exact-getter #\>))
  "<b")

(check-gets (ending-getter char-getter (exact-getter #\>)) "a>" #\a 2)
(check-gets (ending-getter char-getter (exact-getter #\>)) "a>c" #\a 2)
(check-get-raises (ending-getter char-getter (exact-getter #\>)) "a")

(check-gets (eol?-list-getter char-whitespace? (exact-string-getter "foo")) "" '() 0)
(check-gets (eol?-list-getter char-whitespace? (exact-string-getter "foo")) " " '() 0)
(check-gets (eol?-list-getter char-whitespace? (exact-string-getter "foo")) "foo" '("foo") 3)
(check-gets (eol?-list-getter char-whitespace? (exact-string-getter "foo")) "foo bar" '("foo") 3)
(check-gets (eol?-list-getter char-whitespace? (exact-string-getter "foo")) "foofoo bar" '("foo" "foo") 6)

(check-gets newline-getter "\nabc" #\newline 1)
(check-get-raises newline-getter "")
(check-get-raises newline-getter "abc")

(check-gets space-getter " abc" #\space 1)
(check-get-raises space-getter "")
(check-get-raises space-getter "abc")

(check-gets comma-getter ",abc" #\, 1)
(check-get-raises comma-getter "")
(check-get-raises comma-getter "abc")

(check-gets colon-getter ":abc" #\: 1)
(check-get-raises colon-getter "")
(check-get-raises colon-getter "abc")

; uses (source-file-descriptor "test.txt" 0)
(check-gets (apply-getter source-file-descriptor-path sfd-getter) "" "test.txt" 0)
(check-gets (apply-getter source-file-descriptor-checksum sfd-getter) "" 0 0)

(check-get-raises (error-getter "some error" '(some datum)) "")

(check (string? (getter-load! string-getter "getter-test.ss")))
