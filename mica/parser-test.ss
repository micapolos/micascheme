(import
  (prefix (micascheme) %)
  (mica parser))

(check-parses eof "" %eof)

(check-parses char "a" #\a)
(check-parse-error char "")
(check-parse-error char "ab")

(check-parses string "" "")
(check-parses string "a" "a")
(check-parses string "ab\n\\" "ab\n\\")

(check-parses #\a "a" #\a)
(check-parses "foo" "foo" "foo")

(check-parses (prefixed "- " string) "- " "")
(check-parses (prefixed "- " string) "- abc" "abc")
(check-parse-error (prefixed "- " string) "")
(check-parse-error (prefixed "- " string) "-")

(check-parses (suffixed char "!") "a!" #\a)
(check-parse-error (suffixed char "!") "")
(check-parse-error (suffixed char "!") "ab!")
