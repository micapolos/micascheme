(import (scheme) (check) (get) (eof))

(check-gets eof?-getter "" #t 0)
(check-gets eof?-getter "abc" #f 0)

(check-get-raises char-getter "")
(check-gets char-getter "abc" #\a 1)

(check-gets char/eof-getter "" eof 0)
(check-gets char/eof-getter "abc" #\a 1)

(check-gets (exact-char-getter #\a) "abc" #\a 1)
(check-get-raises (exact-char-getter #\a) "")
(check-get-raises (exact-char-getter #\a) "bca")

(check-gets (exact-string-getter "foo") "foo" "foo" 3)
(check-gets (exact-string-getter "foo") "foobar" "foo" 3)
(check-get-raises (exact-string-getter "foo") "")
(check-get-raises (exact-string-getter "foo") "fo")

(check-gets peek-char/eof-getter "" eof 0)
(check-gets peek-char/eof-getter "abc" #\a 0)

(check-gets
  (append-getter char/eof-getter char/eof-getter)
  "abc"
  '(#\a #\b)
  2)

(check-gets
  (append-getter char/eof-getter (char-ungetter #\a))
  "abc"
  '(#\a #\a)
  0)

(check-gets
  (append-getter char/eof-getter (char-ungetter #\a) char/eof-getter)
  "abc"
  '(#\a #\a #\a)
  1)

(check-gets datum/eof-getter "" eof 0)
(check-gets datum/eof-getter "   " eof 3)
(check-gets datum/eof-getter "(foo bar)" '(foo bar) 9)
(check-gets datum/eof-getter "(foo bar) (zoo zar)" '(foo bar) 9)

(check-gets (skip-until-getter char-whitespace?) "" eof 0)
(check-gets (skip-until-getter char-whitespace?) "   " eof 3)
(check-gets (skip-until-getter char-whitespace?) "   foo" #\f 3)

(check-gets (test?-string-getter char-alphabetic?) "" "" 0)
(check-gets (test?-string-getter char-alphabetic?) "foo" "foo" 3)
(check-gets (test?-string-getter char-alphabetic?) "foo123" "foo" 3)

(check-gets bfp-getter "" 0 0)
(check-gets
  (append-getter char-getter char-getter bfp-getter char-getter)
  "abcd"
  '(#\a #\b 2 #\c)
  3)

(check-gets (ending-getter char-getter char-getter) "ab" #\a 2)
(check-gets (ending-getter char-getter char-getter) "abc" #\a 2)
(check-get-raises (ending-getter char-getter char-getter) "a")

(check-gets (list-getter datum/eof-getter) "" '() 0)
(check-gets (list-getter datum/eof-getter) "10 20 30" '(10 20 30) 8)
(check-gets (list-getter datum/eof-getter) "   " '() 3)
(check-gets (list-getter datum/eof-getter) "  10  20 30  " '(10 20 30) 13)

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
