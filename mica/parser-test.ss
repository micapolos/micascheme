(import
  (prefix (micascheme) %)
  (mica parser))

(check-parses eof "" %eof)
(check-parse-error eof "a")

(check-parses char "a" #\a)
(check-parse-error char "")
(check-parse-error char "ab")

(check-parses (?char %char-numeric?) "1" #\1)
(check-parse-error (?char %char-numeric?) "a")

(check-parses digit "1" 1)
(check-parse-error digit "")
(check-parse-error digit "a")

(check-parses string "" "")
(check-parses string "a" "a")
(check-parses string "ab\n\\" "ab\n\\")

(check-parses #\a "a" #\a)
(check-parses "" "" "")
(check-parses "foo" "foo" "foo")

(check-parses (prefixed "- " string) "- " "")
(check-parses (prefixed "- " string) "- abc" "abc")
(check-parse-error (prefixed "- " string) "")
(check-parse-error (prefixed "- " string) "-")

(check-parses (suffixed char "!") "a!" #\a)
(check-parse-error (suffixed char "!") "")
(check-parse-error (suffixed char "!") "ab!")

(check-parses (optional alphabetic-string) "" #f)
(check-parses (optional alphabetic-string) "abc" "abc")
(check-parse-error (optional alphabetic-string) "ab1")

(check-parses (optional numeric-string) "" #f)
(check-parses (optional numeric-string) "123" "123")
(check-parse-error (optional numeric-string) "12a")

(check-parses (map numeric-string %string->number) "123" 123)
(check-parses (map numeric-string %string->number %-) "123" -123)

(check-parses (indented string) "" "")
(check-parse-error (indented string) "abc")
(check-parse-error (indented string) " abc")
(check-parses (indented string) "  abc" "abc")
(check-parses (indented string) "  abc\n" "abc\n")
(check-parse-error (indented string) "  abc\n ")
(check-parse-error (indented string) "  abc\n  ")
(check-parses (indented string) "  abc\n  def" "abc\ndef")

(check-parses
  (one-of
    alphabetic-string
    (map numeric-string %string->number))
  "abc"
  "abc")

(check-parses
  (one-of
    alphabetic-string
    (map numeric-string %string->number))
  "123"
  123)

(check-parse-error
  (one-of
    alphabetic-string
    (map numeric-string %string->number))
  "123!")

(check-parses (list digit) "" (%list))
(check-parses (list digit) "1" (%list 1))
(check-parses (list digit) "123" (%list 1 2 3))
(check-parse-error (list digit) "a")
(check-parse-error (list digit) "1a")

(%lets
  (number
    (map
      (apply %append
        (apply %prepend
          (optional (one-of #\+ #\-))
          (non-empty-list numeric-char))
        (map
          (optional (apply %prepend #\. (non-empty-list numeric-char)))
          (%default %null)))
      %?filter
      %list->string
      %string->number))
  (%run
    (check-parses number "123" 123)
    (check-parses number "+123" 123)
    (check-parses number "-123" -123)
    (check-parses number "-123.45" -123.45)))

