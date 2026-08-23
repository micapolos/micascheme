(import (scheme) (check) (string))

(check (equal? (empty-string) ""))
(check (string-empty? ""))
(check (not (string-empty? " ")))

(check (equal? (lines-string) ""))
(check (equal? (lines-string "foo") "foo\n"))
(check (equal? (lines-string "foo" "bar") "foo\nbar\n"))
(check (equal? (apply lines-string (list "foo" "bar")) "foo\nbar\n"))

(check (equal? (string->ascii "\x0;\x7f;\xff;\x1234;") (bytevector 0 #x7f #xff #x34)))

; --- string-split

;; 1. Empty Source String
(check (equal? (string-split "" "->") (list "")))
(check (equal? (string-split "" "") '()))

;; 2. Basic Single & Multi-character Delimiters
(check (equal? (string-split "a->b" "->") (list "a" "b")))
(check (equal? (string-split "a,b,c" ",") (list "a" "b" "c")))
(check (equal? (string-split "hello" "x") (list "hello")))

;; 3. Leading & Trailing Matches
(check (equal? (string-split "->a" "->") (list "" "a")))
(check (equal? (string-split "a->" "->") (list "a" "")))
(check (equal? (string-split "->a->b->" "->") (list "" "a" "b" "")))

;; 4. Adjacent Delimiters (Preserving Empty Substrings)
(check (equal? (string-split "a->->b" "->") (list "a" "" "b")))
(check (equal? (string-split "a,,b" ",") (list "a" "" "b")))
(check (equal? (string-split "->" "->") (list "" "")))
(check (equal? (string-split "->->" "->") (list "" "" "")))

;; 5. Empty Delimiter (Splits into Individual Character Strings)
(check (equal? (string-split "abc" "") (list "a" "b" "c")))
(check (equal? (string-split "a" "") (list "a")))

;; 6. Source String Shorter Than Delimiter
(check (equal? (string-split "a" "->") (list "a")))
(check (equal? (string-split "short" "longer-delimiter") (list "short")))

;; 7. Non-overlapping Match Precedence (e.g. splitting "aaa" by "aa")
(check (equal? (string-split "aaa" "aa") (list "" "a")))
(check (equal? (string-split "aaaa" "aa") (list "" "" "")))
