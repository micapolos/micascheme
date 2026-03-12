(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica parser)
  (getter)
  (prefix (annotation) %))

(check-parser eof
  (ok "" %eof)
  (error "a"))

(check-parser char
  (ok "a" #\a)
  (error "")
  (error "ab"))

(check-parser (?char %char-numeric?)
  (ok "1" #\1)
  (error "a"))

(check-parser digit
  (ok "1" 1)
  (error "")
  (error "a"))

(check-parser (category-char Ll Lu Nd)
  (ok "a" #\a)
  (ok "A" #\A)
  (ok "1" #\1)
  (error " "))

(check-parser (range-char #\a #\z)
  (ok "a" #\a)
  (ok "x" #\x)
  (ok "z" #\z)
  (error "A"))

(check-parser (char>= #\c)
  (ok "c" #\c)
  (ok "d" #\d)
  (error "b"))

(check-parser (char<= #\c)
  (ok "c" #\c)
  (ok "b" #\b)
  (error "d"))

(check-parser (first-char (> #\a) (not #\c #\e) alphabetic-string)
  (error "12")
  (error "ab")
  (ok "bc" "bc")
  (error "cd")
  (ok "de" "de")
  (error "ef"))

(check-parser string
  (ok "" "")
  (ok "a" "a")
  (ok "ab\n\\" "ab\n\\"))

(check-parser (string)
  (ok "" "")
  (error "a"))

(check-parser (string #\a #\b)
  (ok "ab" "ab")
  (error "a")
  (error "abc"))

(check-parser #\a (ok "a" #\a))
(check-parser (char a) (ok "a" #\a))
(check-parser (char colon) (ok ":" #\:))
(check-parser (char space) (ok " " #\space))

(check-parser "" (ok "" ""))
(check-parser "foo" (ok "foo" "foo"))

(check-parser (prefixed "- " string)
  (ok "- " "")
  (ok "- abc" "abc")
  (error "")
  (error "-"))

(check-parser (suffixed char "!")
  (ok "a!" #\a)
  (error "")
  (error "ab!"))

(check-parser (wrapped "(" char ")")
  (ok "(a)" #\a)
  (error "a)")
  (error "(a")
  (error "a"))

(check-parser (optional alphabetic-string)
  (ok "" #f)
  (ok "abc" "abc")
  (error "ab1"))

(check-parser (optional numeric-string)
  (ok "" #f)
  (ok "123" "123")
  (error "12a"))

(check-parses (map numeric-string %string->number) "123" 123)
(check-parses (map numeric-string %string->number %-) "123" -123)

(check-parser (indented string)
  (ok "" "")
  (error "abc")
  (error " abc")
  (ok "  abc" "abc")
  (ok "  abc\n" "abc\n")
  (error "  abc\n ")
  (error "  abc\n  ")
  (ok "  abc\n  def" "abc\ndef"))

(check-parser (one-of alphabetic-string (map numeric-string %string->number))
  (ok "abc" "abc")
  (ok "123" 123)
  (error "123!"))

(check-parser (or (optional alphabetic-char) (optional (string numeric-char)))
  (ok "a" #\a)
  (ok "2" "2")
  (error "+"))

(check-parser (list digit)
  (ok "" (%list))
  (ok "1" (%list 1))
  (ok "123" (%list 1 2 3))
  (error "a")
  (error "1a"))

(check-parser (non-empty-separated ", " alphabetic-char)
  (ok "a" (%list #\a))
  (ok "a, b" (%list #\a #\b))
  (ok "a, b, c" (%list #\a #\b #\c))
  (error "")
  (error "a,")
  (error "a, ")
  (error "1"))

(check-parser (separated ", " alphabetic-char)
  (ok "" (%list))
  (ok "a" (%list #\a))
  (ok "a, b" (%list #\a #\b))
  (ok "a, b, c" (%list #\a #\b #\c))
  (error "a,")
  (error "a, ")
  (error "1"))

(check-parser null
  (ok "" %null)
  (error "a"))

(check-parser
  (map
    (append
      (prepend
        (optional (one-of #\+ #\-))
        (non-empty-list numeric-char))
      (or
        (optional (prepend #\. (non-empty-list numeric-char)))
        null))
    %?filter
    %list->string
    %string->number)
  (ok "123" 123)
  (ok "+123" 123)
  (ok "-123" -123)
  (ok "-123.45" -123.45))

(check-parser (string->datum string)
  (ok "foo" 'foo)
  (ok "(a b)" '(a b)))

(check-parser (annotation string)
  (ok "foo" (%stripped-annotation "foo" (test-source-object 0 3))))

(check-parser
  (switch alphabetic-char
    (((%partial %char=? #\a) $a)
      (prefixed "-1" (return $a)))
    (((%partial %char=? #\b) $b)
      (prefixed "-2" (return $b)))
    ((else $other)
      (prefixed "-3" (return $other))))
  (ok "a-1" #\a)
  (ok "b-2" #\b)
  (ok "c-3" #\c)
  (ok "d-3" #\d))
