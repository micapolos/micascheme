(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica reader)
  (getter)
  (prefix (annotation) %))

(check-reader (error "dupa" 123)
  (error "")
  (error "dupa")
  (error "123"))

(check-reader eof
  (ok "" %eof)
  (error "a"))

(check-reader char
  (ok "a" #\a)
  (error "")
  (error "ab"))

(check-reader (?char %char-numeric?)
  (ok "1" #\1)
  (error "a"))

(check-reader digit
  (ok "1" 1)
  (error "")
  (error "a"))

(check-reader (category-char Ll Lu Nd)
  (ok "a" #\a)
  (ok "A" #\A)
  (ok "1" #\1)
  (error " "))

(check-reader (range-char #\a #\z)
  (ok "a" #\a)
  (ok "x" #\x)
  (ok "z" #\z)
  (error "A"))

(check-reader (char>= #\c)
  (ok "c" #\c)
  (ok "d" #\d)
  (error "b"))

(check-reader (char<= #\c)
  (ok "c" #\c)
  (ok "b" #\b)
  (error "d"))

(check-reader (first-char (> #\a) (not #\c #\e) alphabetic-string)
  (error "12")
  (error "ab")
  (ok "bc" "bc")
  (error "cd")
  (ok "de" "de")
  (error "ef"))

(check-reader string
  (ok "" "")
  (ok "a" "a")
  (ok "ab\n\\" "ab\n\\"))

(check-reader (string)
  (ok "" "")
  (error "a"))

(check-reader (string #\a #\b)
  (ok "ab" "ab")
  (error "a")
  (error "abc"))

(check-reader #\a (ok "a" #\a))
(check-reader (char a) (ok "a" #\a))
(check-reader (char colon) (ok ":" #\:))
(check-reader (char space) (ok " " #\space))

(check-reader "" (ok "" ""))
(check-reader "foo" (ok "foo" "foo"))

(check-reader (prefixed "- " string)
  (ok "- " "")
  (ok "- abc" "abc")
  (error "")
  (error "-"))

(check-reader (suffixed char "!")
  (ok "a!" #\a)
  (error "")
  (error "ab!"))

(check-reader (wrapped "(" char ")")
  (ok "(a)" #\a)
  (error "a)")
  (error "(a")
  (error "a"))

(check-reader (optional alphabetic-string)
  (ok "" #f)
  (ok "abc" "abc")
  (error "ab1"))

(check-reader (optional numeric-string)
  (ok "" #f)
  (ok "123" "123")
  (error "12a"))

(check-reads (map numeric-string %string->number) "123" 123)
(check-reads (map numeric-string %string->number %-) "123" -123)

(check-reader (indented string)
  (ok "" "")
  (error "abc")
  (error " abc")
  (ok "  abc" "abc")
  (ok "  abc\n" "abc\n")
  (error "  abc\n ")
  (error "  abc\n  ")
  (ok "  abc\n  def" "abc\ndef"))

(check-reader (one-of alphabetic-string (map numeric-string %string->number))
  (ok "abc" "abc")
  (ok "123" 123)
  (error "123!"))

(check-reader (or (optional alphabetic-char) (optional (string numeric-char)))
  (ok "a" #\a)
  (ok "2" "2")
  (error "+"))

(check-reader (list digit)
  (ok "" (%list))
  (ok "1" (%list 1))
  (ok "123" (%list 1 2 3))
  (error "a")
  (error "1a"))

(check-reader (non-empty-separated ", " alphabetic-char)
  (ok "a" (%list #\a))
  (ok "a, b" (%list #\a #\b))
  (ok "a, b, c" (%list #\a #\b #\c))
  (error "")
  (error "a,")
  (error "a, ")
  (error "1"))

(check-reader (separated ", " alphabetic-char)
  (ok "" (%list))
  (ok "a" (%list #\a))
  (ok "a, b" (%list #\a #\b))
  (ok "a, b, c" (%list #\a #\b #\c))
  (error "a,")
  (error "a, ")
  (error "1"))

(check-reader null
  (ok "" %null)
  (error "a"))

(check-reader
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

(check-reader (string->datum string)
  (ok "foo" 'foo)
  (ok "(a b)" '(a b)))

(check-reader (annotation string)
  (ok "foo" (%stripped-annotation "foo" (test-source-object 0 3))))

(check-reader (list-annotation (list (annotation char)))
  (ok "foo"
    (%list-annotation
      (%list
        (%stripped-annotation #\f (test-source-object 0 1))
        (%stripped-annotation #\o (test-source-object 1 2))
        (%stripped-annotation #\o (test-source-object 2 3)))
      (test-source-object 0 3))))

(check-reader
  (lets
    ($char #\a)
    ($string "foo")
    (return (%list $char $string)))
  (ok "afoo" '(#\a "foo")))

(check-reader
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
